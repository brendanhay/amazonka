{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.PostCommentReply
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts a comment in reply to an existing comment on a comparison between commits or a pull request.
--
--
module Network.AWS.CodeCommit.PostCommentReply
    (
    -- * Creating a Request
      postCommentReply
    , PostCommentReply
    -- * Request Lenses
    , pcrClientRequestToken
    , pcrInReplyTo
    , pcrContent

    -- * Destructuring the Response
    , postCommentReplyResponse
    , PostCommentReplyResponse
    -- * Response Lenses
    , pcrrsComment
    , pcrrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'postCommentReply' smart constructor.
data PostCommentReply = PostCommentReply'
  { _pcrClientRequestToken :: !(Maybe Text)
  , _pcrInReplyTo          :: !Text
  , _pcrContent            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PostCommentReply' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcrClientRequestToken' - A unique, client-generated idempotency token that when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request will return information about the initial request that used that token.
--
-- * 'pcrInReplyTo' - The system-generated ID of the comment to which you want to reply. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
--
-- * 'pcrContent' - The contents of your reply to a comment.
postCommentReply
    :: Text -- ^ 'pcrInReplyTo'
    -> Text -- ^ 'pcrContent'
    -> PostCommentReply
postCommentReply pInReplyTo_ pContent_ =
  PostCommentReply'
    { _pcrClientRequestToken = Nothing
    , _pcrInReplyTo = pInReplyTo_
    , _pcrContent = pContent_
    }


-- | A unique, client-generated idempotency token that when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request will return information about the initial request that used that token.
pcrClientRequestToken :: Lens' PostCommentReply (Maybe Text)
pcrClientRequestToken = lens _pcrClientRequestToken (\ s a -> s{_pcrClientRequestToken = a})

-- | The system-generated ID of the comment to which you want to reply. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
pcrInReplyTo :: Lens' PostCommentReply Text
pcrInReplyTo = lens _pcrInReplyTo (\ s a -> s{_pcrInReplyTo = a})

-- | The contents of your reply to a comment.
pcrContent :: Lens' PostCommentReply Text
pcrContent = lens _pcrContent (\ s a -> s{_pcrContent = a})

instance AWSRequest PostCommentReply where
        type Rs PostCommentReply = PostCommentReplyResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 PostCommentReplyResponse' <$>
                   (x .?> "comment") <*> (pure (fromEnum s)))

instance Hashable PostCommentReply where

instance NFData PostCommentReply where

instance ToHeaders PostCommentReply where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.PostCommentReply" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PostCommentReply where
        toJSON PostCommentReply'{..}
          = object
              (catMaybes
                 [("clientRequestToken" .=) <$>
                    _pcrClientRequestToken,
                  Just ("inReplyTo" .= _pcrInReplyTo),
                  Just ("content" .= _pcrContent)])

instance ToPath PostCommentReply where
        toPath = const "/"

instance ToQuery PostCommentReply where
        toQuery = const mempty

-- | /See:/ 'postCommentReplyResponse' smart constructor.
data PostCommentReplyResponse = PostCommentReplyResponse'
  { _pcrrsComment        :: !(Maybe Comment)
  , _pcrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PostCommentReplyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcrrsComment' - Information about the reply to a comment.
--
-- * 'pcrrsResponseStatus' - -- | The response status code.
postCommentReplyResponse
    :: Int -- ^ 'pcrrsResponseStatus'
    -> PostCommentReplyResponse
postCommentReplyResponse pResponseStatus_ =
  PostCommentReplyResponse'
    {_pcrrsComment = Nothing, _pcrrsResponseStatus = pResponseStatus_}


-- | Information about the reply to a comment.
pcrrsComment :: Lens' PostCommentReplyResponse (Maybe Comment)
pcrrsComment = lens _pcrrsComment (\ s a -> s{_pcrrsComment = a})

-- | -- | The response status code.
pcrrsResponseStatus :: Lens' PostCommentReplyResponse Int
pcrrsResponseStatus = lens _pcrrsResponseStatus (\ s a -> s{_pcrrsResponseStatus = a})

instance NFData PostCommentReplyResponse where
