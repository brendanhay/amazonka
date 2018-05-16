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
-- Module      : Network.AWS.CodeCommit.UpdateComment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the contents of a comment.
--
--
module Network.AWS.CodeCommit.UpdateComment
    (
    -- * Creating a Request
      updateComment
    , UpdateComment
    -- * Request Lenses
    , ucCommentId
    , ucContent

    -- * Destructuring the Response
    , updateCommentResponse
    , UpdateCommentResponse
    -- * Response Lenses
    , ucrsComment
    , ucrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateComment' smart constructor.
data UpdateComment = UpdateComment'
  { _ucCommentId :: !Text
  , _ucContent   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateComment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucCommentId' - The system-generated ID of the comment you want to update. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
--
-- * 'ucContent' - The updated content with which you want to replace the existing content of the comment.
updateComment
    :: Text -- ^ 'ucCommentId'
    -> Text -- ^ 'ucContent'
    -> UpdateComment
updateComment pCommentId_ pContent_ =
  UpdateComment' {_ucCommentId = pCommentId_, _ucContent = pContent_}


-- | The system-generated ID of the comment you want to update. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
ucCommentId :: Lens' UpdateComment Text
ucCommentId = lens _ucCommentId (\ s a -> s{_ucCommentId = a})

-- | The updated content with which you want to replace the existing content of the comment.
ucContent :: Lens' UpdateComment Text
ucContent = lens _ucContent (\ s a -> s{_ucContent = a})

instance AWSRequest UpdateComment where
        type Rs UpdateComment = UpdateCommentResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 UpdateCommentResponse' <$>
                   (x .?> "comment") <*> (pure (fromEnum s)))

instance Hashable UpdateComment where

instance NFData UpdateComment where

instance ToHeaders UpdateComment where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.UpdateComment" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateComment where
        toJSON UpdateComment'{..}
          = object
              (catMaybes
                 [Just ("commentId" .= _ucCommentId),
                  Just ("content" .= _ucContent)])

instance ToPath UpdateComment where
        toPath = const "/"

instance ToQuery UpdateComment where
        toQuery = const mempty

-- | /See:/ 'updateCommentResponse' smart constructor.
data UpdateCommentResponse = UpdateCommentResponse'
  { _ucrsComment        :: !(Maybe Comment)
  , _ucrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCommentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucrsComment' - Information about the updated comment.
--
-- * 'ucrsResponseStatus' - -- | The response status code.
updateCommentResponse
    :: Int -- ^ 'ucrsResponseStatus'
    -> UpdateCommentResponse
updateCommentResponse pResponseStatus_ =
  UpdateCommentResponse'
    {_ucrsComment = Nothing, _ucrsResponseStatus = pResponseStatus_}


-- | Information about the updated comment.
ucrsComment :: Lens' UpdateCommentResponse (Maybe Comment)
ucrsComment = lens _ucrsComment (\ s a -> s{_ucrsComment = a})

-- | -- | The response status code.
ucrsResponseStatus :: Lens' UpdateCommentResponse Int
ucrsResponseStatus = lens _ucrsResponseStatus (\ s a -> s{_ucrsResponseStatus = a})

instance NFData UpdateCommentResponse where
