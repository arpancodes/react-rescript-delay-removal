// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Post from "./post.bs.js";
import * as Curry from "bs-platform/lib/es6/curry.js";
import * as React from "react";
import * as Belt_Array from "bs-platform/lib/es6/belt_Array.js";
import * as Belt_MapString from "bs-platform/lib/es6/belt_MapString.js";

function s(prim) {
  return prim;
}

function deleteLater(state, post, timeoutId) {
  console.log(post, timeoutId);
  var updatedForDeletion = Belt_MapString.set(state.forDeletion, Post.id(post), timeoutId);
  var updatedState_posts = state.posts;
  var updatedState = {
    posts: updatedState_posts,
    forDeletion: updatedForDeletion
  };
  console.log("UPDATED STATE", updatedState);
  return updatedState;
}

function deleteAbort(state, post) {
  console.log("Cancelled deletion of", post);
  var timeoutId = Belt_MapString.getExn(state.forDeletion, Post.id(post));
  clearTimeout(timeoutId);
  var updatedForDeletion = Belt_MapString.remove(state.forDeletion, Post.id(post));
  var updatedState_posts = state.posts;
  var updatedState = {
    posts: updatedState_posts,
    forDeletion: updatedForDeletion
  };
  console.log("UPDATED STATE", updatedState);
  return updatedState;
}

function deleteNow(state, post) {
  console.log("deleting", post);
  var timeoutId = Belt_MapString.getExn(state.forDeletion, Post.id(post));
  clearTimeout(timeoutId);
  var updatedForDeletion = Belt_MapString.remove(state.forDeletion, Post.id(post));
  var updatedPosts = Belt_Array.keep(state.posts, (function (curr) {
          return curr.id !== Post.id(post);
        }));
  var updatedState = {
    posts: updatedPosts,
    forDeletion: updatedForDeletion
  };
  console.log("UPDATED STATE", updatedState);
  return updatedState;
}

function reducer(state, action) {
  switch (action.TAG | 0) {
    case /* DeleteLater */0 :
        return deleteLater(state, action._0, action._1);
    case /* DeleteAbort */1 :
        return deleteAbort(state, action._0);
    case /* DeleteNow */2 :
        return deleteNow(state, action._0);
    
  }
}

var initialState = {
  posts: Post.examples,
  forDeletion: undefined
};

function PostFeed$PostView(Props) {
  var postText = Props.postText;
  var postId = Props.postId;
  var postTitle = Props.postTitle;
  var postAuthor = Props.postAuthor;
  var removeHandler = Props.removeHandler;
  var texts = Belt_Array.mapWithIndex(postText, (function (i, text) {
          return React.createElement("p", {
                      key: postId + String(i),
                      className: "mb-1 text-sm"
                    }, text);
        }));
  return React.createElement("div", {
              className: "bg-green-700 hover:bg-green-900 text-gray-300 hover:text-gray-100 px-8 py-4 mb-4"
            }, React.createElement("h2", {
                  className: "text-2xl mb-1"
                }, postTitle), React.createElement("h3", {
                  className: "mb-4"
                }, postAuthor), texts, React.createElement("button", {
                  className: "mr-4 mt-4 bg-red-500 hover:bg-red-900 text-white py-2 px-4",
                  onClick: removeHandler
                }, "Remove this post"));
}

var PostView = {
  make: PostFeed$PostView
};

function PostFeed$DeleteNotificationView(Props) {
  var postTitle = Props.postTitle;
  var postAuthor = Props.postAuthor;
  var removeImmediateHandler = Props.removeImmediateHandler;
  var removeAbortHandler = Props.removeAbortHandler;
  return React.createElement("div", {
              className: "relative bg-yellow-100 px-8 py-4 mb-4 h-40"
            }, React.createElement("p", {
                  className: "text-center white mb-1"
                }, "This post from " + postTitle + " by " + postAuthor + " will be permanently removed in 10 seconds."), React.createElement("div", {
                  className: "flex justify-center"
                }, React.createElement("button", {
                      className: "mr-4 mt-4 bg-yellow-500 hover:bg-yellow-900 text-white py-2 px-4",
                      onClick: removeAbortHandler
                    }, "Restore"), React.createElement("button", {
                      className: "mr-4 mt-4 bg-red-500 hover:bg-red-900 text-white py-2 px-4",
                      onClick: removeImmediateHandler
                    }, "Delete Immediately")), React.createElement("div", {
                  className: "bg-red-500 h-2 w-full absolute top-0 left-0 progress"
                }));
}

var DeleteNotificationView = {
  make: PostFeed$DeleteNotificationView
};

function PostFeed(Props) {
  var match = React.useReducer(reducer, initialState);
  var dispatch = match[1];
  var state = match[0];
  var posts = Belt_Array.map(state.posts, (function (post) {
          if (Belt_MapString.has(state.forDeletion, post.id)) {
            return React.createElement(PostFeed$DeleteNotificationView, {
                        postTitle: post.title,
                        postAuthor: post.author,
                        removeImmediateHandler: (function (_mouseEtv) {
                            return Curry._1(dispatch, {
                                        TAG: /* DeleteNow */2,
                                        _0: post
                                      });
                          }),
                        removeAbortHandler: (function (_mouseEtv) {
                            return Curry._1(dispatch, {
                                        TAG: /* DeleteAbort */1,
                                        _0: post
                                      });
                          }),
                        key: post.id
                      });
          } else {
            return React.createElement(PostFeed$PostView, {
                        postText: post.text,
                        postId: post.id,
                        postTitle: post.title,
                        postAuthor: post.author,
                        removeHandler: (function (_mouseEvt) {
                            var timeout = setTimeout((function (param) {
                                    return Curry._1(dispatch, {
                                                TAG: /* DeleteNow */2,
                                                _0: post
                                              });
                                  }), 10000);
                            return Curry._1(dispatch, {
                                        TAG: /* DeleteLater */0,
                                        _0: post,
                                        _1: timeout
                                      });
                          }),
                        key: post.id
                      });
          }
        }));
  return React.createElement("div", {
              className: "max-w-3xl mx-auto mt-8 relative"
            }, posts);
}

var make = PostFeed;

export {
  s ,
  deleteLater ,
  deleteAbort ,
  deleteNow ,
  reducer ,
  initialState ,
  PostView ,
  DeleteNotificationView ,
  make ,
  
}
/* Post Not a pure module */
