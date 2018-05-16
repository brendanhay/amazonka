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
-- Module      : Network.AWS.WorkDocs.CreateComment
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new comment to the specified document version.
--
--
module Network.AWS.WorkDocs.CreateComment
    (
    -- * Creating a Request
      createComment
    , CreateComment
    -- * Request Lenses
    , ccNotifyCollaborators
    , ccAuthenticationToken
    , ccVisibility
    , ccThreadId
    , ccParentId
    , ccDocumentId
    , ccVersionId
    , ccText

    -- * Destructuring the Response
    , createCommentResponse
    , CreateCommentResponse
    -- * Response Lenses
    , ccrsComment
    , ccrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'createComment' smart constructor.
data CreateComment = CreateComment'
  { _ccNotifyCollaborators :: !(Maybe Bool)
  , _ccAuthenticationToken :: !(Maybe (Sensitive Text))
  , _ccVisibility          :: !(Maybe CommentVisibilityType)
  , _ccThreadId            :: !(Maybe Text)
  , _ccParentId            :: !(Maybe Text)
  , _ccDocumentId          :: !Text
  , _ccVersionId           :: !Text
  , _ccText                :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateComment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccNotifyCollaborators' - Set this parameter to TRUE to send an email out to the document collaborators after the comment is created.
--
-- * 'ccAuthenticationToken' - Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
--
-- * 'ccVisibility' - The visibility of the comment. Options are either PRIVATE, where the comment is visible only to the comment author and document owner and co-owners, or PUBLIC, where the comment is visible to document owners, co-owners, and contributors.
--
-- * 'ccThreadId' - The ID of the root comment in the thread.
--
-- * 'ccParentId' - The ID of the parent comment.
--
-- * 'ccDocumentId' - The ID of the document.
--
-- * 'ccVersionId' - The ID of the document version.
--
-- * 'ccText' - The text of the comment.
createComment
    :: Text -- ^ 'ccDocumentId'
    -> Text -- ^ 'ccVersionId'
    -> Text -- ^ 'ccText'
    -> CreateComment
createComment pDocumentId_ pVersionId_ pText_ =
  CreateComment'
    { _ccNotifyCollaborators = Nothing
    , _ccAuthenticationToken = Nothing
    , _ccVisibility = Nothing
    , _ccThreadId = Nothing
    , _ccParentId = Nothing
    , _ccDocumentId = pDocumentId_
    , _ccVersionId = pVersionId_
    , _ccText = _Sensitive # pText_
    }


-- | Set this parameter to TRUE to send an email out to the document collaborators after the comment is created.
ccNotifyCollaborators :: Lens' CreateComment (Maybe Bool)
ccNotifyCollaborators = lens _ccNotifyCollaborators (\ s a -> s{_ccNotifyCollaborators = a})

-- | Amazon WorkDocs authentication token. Do not set this field when using administrative API actions, as in accessing the API using AWS credentials.
ccAuthenticationToken :: Lens' CreateComment (Maybe Text)
ccAuthenticationToken = lens _ccAuthenticationToken (\ s a -> s{_ccAuthenticationToken = a}) . mapping _Sensitive

-- | The visibility of the comment. Options are either PRIVATE, where the comment is visible only to the comment author and document owner and co-owners, or PUBLIC, where the comment is visible to document owners, co-owners, and contributors.
ccVisibility :: Lens' CreateComment (Maybe CommentVisibilityType)
ccVisibility = lens _ccVisibility (\ s a -> s{_ccVisibility = a})

-- | The ID of the root comment in the thread.
ccThreadId :: Lens' CreateComment (Maybe Text)
ccThreadId = lens _ccThreadId (\ s a -> s{_ccThreadId = a})

-- | The ID of the parent comment.
ccParentId :: Lens' CreateComment (Maybe Text)
ccParentId = lens _ccParentId (\ s a -> s{_ccParentId = a})

-- | The ID of the document.
ccDocumentId :: Lens' CreateComment Text
ccDocumentId = lens _ccDocumentId (\ s a -> s{_ccDocumentId = a})

-- | The ID of the document version.
ccVersionId :: Lens' CreateComment Text
ccVersionId = lens _ccVersionId (\ s a -> s{_ccVersionId = a})

-- | The text of the comment.
ccText :: Lens' CreateComment Text
ccText = lens _ccText (\ s a -> s{_ccText = a}) . _Sensitive

instance AWSRequest CreateComment where
        type Rs CreateComment = CreateCommentResponse
        request = postJSON workDocs
        response
          = receiveJSON
              (\ s h x ->
                 CreateCommentResponse' <$>
                   (x .?> "Comment") <*> (pure (fromEnum s)))

instance Hashable CreateComment where

instance NFData CreateComment where

instance ToHeaders CreateComment where
        toHeaders CreateComment'{..}
          = mconcat
              ["Authentication" =# _ccAuthenticationToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateComment where
        toJSON CreateComment'{..}
          = object
              (catMaybes
                 [("NotifyCollaborators" .=) <$>
                    _ccNotifyCollaborators,
                  ("Visibility" .=) <$> _ccVisibility,
                  ("ThreadId" .=) <$> _ccThreadId,
                  ("ParentId" .=) <$> _ccParentId,
                  Just ("Text" .= _ccText)])

instance ToPath CreateComment where
        toPath CreateComment'{..}
          = mconcat
              ["/api/v1/documents/", toBS _ccDocumentId,
               "/versions/", toBS _ccVersionId, "/comment"]

instance ToQuery CreateComment where
        toQuery = const mempty

-- | /See:/ 'createCommentResponse' smart constructor.
data CreateCommentResponse = CreateCommentResponse'
  { _ccrsComment        :: !(Maybe Comment)
  , _ccrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCommentResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsComment' - The comment that has been created.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createCommentResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CreateCommentResponse
createCommentResponse pResponseStatus_ =
  CreateCommentResponse'
    {_ccrsComment = Nothing, _ccrsResponseStatus = pResponseStatus_}


-- | The comment that has been created.
ccrsComment :: Lens' CreateCommentResponse (Maybe Comment)
ccrsComment = lens _ccrsComment (\ s a -> s{_ccrsComment = a})

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateCommentResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

instance NFData CreateCommentResponse where
