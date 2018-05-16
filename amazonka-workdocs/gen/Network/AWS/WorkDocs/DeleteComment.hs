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
-- Module      : Network.AWS.WorkDocs.DeleteComment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified comment from the document version.
--
--
module Network.AWS.WorkDocs.DeleteComment
    (
    -- * Creating a Request
      deleteComment
    , DeleteComment
    -- * Request Lenses
    , delAuthenticationToken
    , delDocumentId
    , delVersionId
    , delCommentId

    -- * Destructuring the Response
    , deleteCommentResponse
    , DeleteCommentResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'deleteComment' smart constructor.
data DeleteComment = DeleteComment'
  { _delAuthenticationToken :: !(Maybe (Sensitive Text))
  , _delDocumentId          :: !Text
  , _delVersionId           :: !Text
  , _delCommentId           :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteComment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'delDocumentId' - The ID of the document.
--
-- * 'delVersionId' - The ID of the document version.
--
-- * 'delCommentId' - The ID of the comment.
deleteComment
    :: Text -- ^ 'delDocumentId'
    -> Text -- ^ 'delVersionId'
    -> Text -- ^ 'delCommentId'
    -> DeleteComment
deleteComment pDocumentId_ pVersionId_ pCommentId_ =
  DeleteComment'
    { _delAuthenticationToken = Nothing
    , _delDocumentId = pDocumentId_
    , _delVersionId = pVersionId_
    , _delCommentId = pCommentId_
    }


-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
delAuthenticationToken :: Lens' DeleteComment (Maybe Text)
delAuthenticationToken = lens _delAuthenticationToken (\ s a -> s{_delAuthenticationToken = a}) . mapping _Sensitive

-- | The ID of the document.
delDocumentId :: Lens' DeleteComment Text
delDocumentId = lens _delDocumentId (\ s a -> s{_delDocumentId = a})

-- | The ID of the document version.
delVersionId :: Lens' DeleteComment Text
delVersionId = lens _delVersionId (\ s a -> s{_delVersionId = a})

-- | The ID of the comment.
delCommentId :: Lens' DeleteComment Text
delCommentId = lens _delCommentId (\ s a -> s{_delCommentId = a})

instance AWSRequest DeleteComment where
        type Rs DeleteComment = DeleteCommentResponse
        request = delete workDocs
        response = receiveNull DeleteCommentResponse'

instance Hashable DeleteComment where

instance NFData DeleteComment where

instance ToHeaders DeleteComment where
        toHeaders DeleteComment'{..}
          = mconcat
              ["Authentication" =# _delAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToPath DeleteComment where
        toPath DeleteComment'{..}
          = mconcat
              ["/api/v1/documents/", toBS _delDocumentId,
               "/versions/", toBS _delVersionId, "/comment/",
               toBS _delCommentId]

instance ToQuery DeleteComment where
        toQuery = const mempty

-- | /See:/ 'deleteCommentResponse' smart constructor.
data DeleteCommentResponse =
  DeleteCommentResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCommentResponse' with the minimum fields required to make a request.
--
deleteCommentResponse
    :: DeleteCommentResponse
deleteCommentResponse = DeleteCommentResponse'


instance NFData DeleteCommentResponse where
