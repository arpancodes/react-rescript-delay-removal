let s = React.string

open Belt

type state = {posts: array<Post.t>, forDeletion: Map.String.t<Js.Global.timeoutId>}

type action =
  | DeleteLater(Post.t, Js.Global.timeoutId)
  | DeleteAbort(Post.t)
  | DeleteNow(Post.t)

let deleteLater = (state, post, timeoutId) => {
  Js.log2(post, timeoutId)
  let updatedForDeletion = Map.String.set(state.forDeletion, post->Post.id, timeoutId)
  let updatedState = {...state, forDeletion: updatedForDeletion}
  Js.log2("UPDATED STATE", updatedState)
  updatedState
}

let deleteAbort = (state, post) => {
  Js.log2("Cancelled deletion of", post)
  let timeoutId = Map.String.getExn(state.forDeletion, post->Post.id)
  Js.Global.clearTimeout(timeoutId)
  let updatedForDeletion = Map.String.remove(state.forDeletion, post->Post.id)
  let updatedState = {...state, forDeletion: updatedForDeletion}
  Js.log2("UPDATED STATE", updatedState)
  updatedState
}

let deleteNow = (state, post) => {
  Js.log2("deleting", post)
  let timeoutId = Map.String.getExn(state.forDeletion, post->Post.id)
  Js.Global.clearTimeout(timeoutId)
  let updatedForDeletion = Map.String.remove(state.forDeletion, post->Post.id)

  let updatedPosts = Array.keep(state.posts, (curr) => curr.id != post->Post.id)

  let updatedState = {posts:updatedPosts, forDeletion: updatedForDeletion}
  Js.log2("UPDATED STATE", updatedState)
  updatedState
}


let reducer = (state, action) =>
  switch action {
  | DeleteLater(post, timeoutId) => deleteLater(state, post, timeoutId)
  | DeleteAbort(post) => deleteAbort(state, post)
  | DeleteNow(post) => deleteNow(state, post)
  }

let initialState = {posts: Post.examples, forDeletion: Map.String.empty}

module PostView = {
  @react.component
  let make = (~postText, ~postId, ~postTitle, ~postAuthor, ~removeHandler) => {

    let texts = Array.mapWithIndex(postText, (i, text) => {
      <p key={postId ++ Int.toString(i)} className="mb-1 text-sm">
        {s(text)}
      </p>
    })

    <div className="bg-green-700 hover:bg-green-900 text-gray-300 hover:text-gray-100 px-8 py-4 mb-4">
      <h2 className="text-2xl mb-1">{s(postTitle)}</h2>
      <h3 className="mb-4">{s(postAuthor)}</h3>
      {React.array(texts)}
      <button onClick={removeHandler} className="mr-4 mt-4 bg-red-500 hover:bg-red-900 text-white py-2 px-4">{s("Remove this post")}</button>
    </div>
  }
}

module DeleteNotificationView = {
  @react.component
  let make = (~postTitle, ~postAuthor, ~removeImmediateHandler, ~removeAbortHandler) => {
    <div className="relative bg-yellow-100 px-8 py-4 mb-4 h-40">
      <p className="text-center white mb-1">
        {s(`This post from ${postTitle} by ${postAuthor} will be permanently removed in 10 seconds.`)}
      </p>
      <div className="flex justify-center">
        <button onClick={removeAbortHandler} className="mr-4 mt-4 bg-yellow-500 hover:bg-yellow-900 text-white py-2 px-4">{s(`Restore`)}</button
        ><button onClick={removeImmediateHandler} className="mr-4 mt-4 bg-red-500 hover:bg-red-900 text-white py-2 px-4">{s(`Delete Immediately`)}</button>
      </div>
      <div className="bg-red-500 h-2 w-full absolute top-0 left-0 progress"></div>
    </div>
  }
}


@react.component
let make = () => {
  let (state, dispatch) = React.useReducer(reducer, initialState)
  let deleteTimer = (post) => {
    let timeout = Js.Global.setTimeout(() => {
      dispatch(DeleteNow(post))
    }, 10000)
    dispatch(DeleteLater(post, timeout))
  }

  let posts = state.posts->Belt.Array.map((post) => {
      if(state.forDeletion->Map.String.has(post.id)){
        <DeleteNotificationView postTitle={post.title} postAuthor={post.author} removeAbortHandler={_mouseEtv => dispatch(DeleteAbort(post))} removeImmediateHandler={_mouseEtv => dispatch(DeleteNow(post))}  key={post.id} />
      } else {
        <PostView removeHandler={(_mouseEvt) => deleteTimer(post)} postText={post.text} key={post.id} postId={post.id} postTitle={post.title} postAuthor={post.author} />
      }
    })
  <div className="max-w-3xl mx-auto mt-8 relative">
    {React.array(posts)}
  </div>
}
