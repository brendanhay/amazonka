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
-- Module      : Network.AWS.CodeCommit.GetComment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the content of a comment made on a change, file, or commit in a repository.
--
--
module Network.AWS.CodeCommit.GetComment
    (
    -- * Creating a Request
      getComment
    , GetComment
    -- * Request Lenses
    , gcCommentId

    -- * Destructuring the Response
    , getCommentResponse
    , GetCommentResponse
    -- * Response Lenses
    , getrsComment
    , getrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getComment' smart constructor.
newtype GetComment = GetComment'
  { _gcCommentId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetComment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcCommentId' - The unique, system-generated ID of the comment. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
getComment
    :: Text -- ^ 'gcCommentId'
    -> GetComment
getComment pCommentId_ = GetComment' {_gcCommentId = pCommentId_}


-- | The unique, system-generated ID of the comment. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
gcCommentId :: Lens' GetComment Text
gcCommentId = lens _gcCommentId (\ s a -> s{_gcCommentId = a})

instance AWSRequest GetComment where
        type Rs GetComment = GetCommentResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 GetCommentResponse' <$>
                   (x .?> "comment") <*> (pure (fromEnum s)))

instance Hashable GetComment where

instance NFData GetComment where

instance ToHeaders GetComment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.GetComment" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetComment where
        toJSON GetComment'{..}
          = object
              (catMaybes [Just ("commentId" .= _gcCommentId)])

instance ToPath GetComment where
        toPath = const "/"

instance ToQuery GetComment where
        toQuery = const mempty

-- | /See:/ 'getCommentResponse' smart constructor.
data GetCommentResponse = GetCommentResponse'
  { _getrsComment        :: !(Maybe Comment)
  , _getrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCommentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getrsComment' - The contents of the comment.
--
-- * 'getrsResponseStatus' - -- | The response status code.
getCommentResponse
    :: Int -- ^ 'getrsResponseStatus'
    -> GetCommentResponse
getCommentResponse pResponseStatus_ =
  GetCommentResponse'
    {_getrsComment = Nothing, _getrsResponseStatus = pResponseStatus_}


-- | The contents of the comment.
getrsComment :: Lens' GetCommentResponse (Maybe Comment)
getrsComment = lens _getrsComment (\ s a -> s{_getrsComment = a})

-- | -- | The response status code.
getrsResponseStatus :: Lens' GetCommentResponse Int
getrsResponseStatus = lens _getrsResponseStatus (\ s a -> s{_getrsResponseStatus = a})

instance NFData GetCommentResponse where
