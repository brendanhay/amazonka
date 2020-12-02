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
-- Module      : Network.AWS.CodeCommit.DeleteCommentContent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the content of a comment made on a change, file, or commit in a repository.
--
--
module Network.AWS.CodeCommit.DeleteCommentContent
    (
    -- * Creating a Request
      deleteCommentContent
    , DeleteCommentContent
    -- * Request Lenses
    , dccCommentId

    -- * Destructuring the Response
    , deleteCommentContentResponse
    , DeleteCommentContentResponse
    -- * Response Lenses
    , dccrsComment
    , dccrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCommentContent' smart constructor.
newtype DeleteCommentContent = DeleteCommentContent'
  { _dccCommentId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCommentContent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dccCommentId' - The unique, system-generated ID of the comment. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
deleteCommentContent
    :: Text -- ^ 'dccCommentId'
    -> DeleteCommentContent
deleteCommentContent pCommentId_ =
  DeleteCommentContent' {_dccCommentId = pCommentId_}


-- | The unique, system-generated ID of the comment. To get this ID, use 'GetCommentsForComparedCommit' or 'GetCommentsForPullRequest' .
dccCommentId :: Lens' DeleteCommentContent Text
dccCommentId = lens _dccCommentId (\ s a -> s{_dccCommentId = a})

instance AWSRequest DeleteCommentContent where
        type Rs DeleteCommentContent =
             DeleteCommentContentResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 DeleteCommentContentResponse' <$>
                   (x .?> "comment") <*> (pure (fromEnum s)))

instance Hashable DeleteCommentContent where

instance NFData DeleteCommentContent where

instance ToHeaders DeleteCommentContent where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.DeleteCommentContent" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteCommentContent where
        toJSON DeleteCommentContent'{..}
          = object
              (catMaybes [Just ("commentId" .= _dccCommentId)])

instance ToPath DeleteCommentContent where
        toPath = const "/"

instance ToQuery DeleteCommentContent where
        toQuery = const mempty

-- | /See:/ 'deleteCommentContentResponse' smart constructor.
data DeleteCommentContentResponse = DeleteCommentContentResponse'
  { _dccrsComment        :: !(Maybe Comment)
  , _dccrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCommentContentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dccrsComment' - Information about the comment you just deleted.
--
-- * 'dccrsResponseStatus' - -- | The response status code.
deleteCommentContentResponse
    :: Int -- ^ 'dccrsResponseStatus'
    -> DeleteCommentContentResponse
deleteCommentContentResponse pResponseStatus_ =
  DeleteCommentContentResponse'
    {_dccrsComment = Nothing, _dccrsResponseStatus = pResponseStatus_}


-- | Information about the comment you just deleted.
dccrsComment :: Lens' DeleteCommentContentResponse (Maybe Comment)
dccrsComment = lens _dccrsComment (\ s a -> s{_dccrsComment = a})

-- | -- | The response status code.
dccrsResponseStatus :: Lens' DeleteCommentContentResponse Int
dccrsResponseStatus = lens _dccrsResponseStatus (\ s a -> s{_dccrsResponseStatus = a})

instance NFData DeleteCommentContentResponse where
