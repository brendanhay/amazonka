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
-- Module      : Network.AWS.CodeCommit.PostCommentForPullRequest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Posts a comment on a pull request.
--
--
module Network.AWS.CodeCommit.PostCommentForPullRequest
    (
    -- * Creating a Request
      postCommentForPullRequest
    , PostCommentForPullRequest
    -- * Request Lenses
    , pcfprLocation
    , pcfprClientRequestToken
    , pcfprPullRequestId
    , pcfprRepositoryName
    , pcfprBeforeCommitId
    , pcfprAfterCommitId
    , pcfprContent

    -- * Destructuring the Response
    , postCommentForPullRequestResponse
    , PostCommentForPullRequestResponse
    -- * Response Lenses
    , pcfprrsBeforeBlobId
    , pcfprrsLocation
    , pcfprrsAfterCommitId
    , pcfprrsPullRequestId
    , pcfprrsAfterBlobId
    , pcfprrsBeforeCommitId
    , pcfprrsRepositoryName
    , pcfprrsComment
    , pcfprrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'postCommentForPullRequest' smart constructor.
data PostCommentForPullRequest = PostCommentForPullRequest'
  { _pcfprLocation           :: !(Maybe Location)
  , _pcfprClientRequestToken :: !(Maybe Text)
  , _pcfprPullRequestId      :: !Text
  , _pcfprRepositoryName     :: !Text
  , _pcfprBeforeCommitId     :: !Text
  , _pcfprAfterCommitId      :: !Text
  , _pcfprContent            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PostCommentForPullRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcfprLocation' - The location of the change where you want to post your comment. If no location is provided, the comment will be posted as a general comment on the pull request difference between the before commit ID and the after commit ID.
--
-- * 'pcfprClientRequestToken' - A unique, client-generated idempotency token that when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request will return information about the initial request that used that token.
--
-- * 'pcfprPullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
--
-- * 'pcfprRepositoryName' - The name of the repository where you want to post a comment on a pull request.
--
-- * 'pcfprBeforeCommitId' - The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was created.
--
-- * 'pcfprAfterCommitId' - The full commit ID of the commit in the source branch that is the current tip of the branch for the pull request when you post the comment.
--
-- * 'pcfprContent' - The content of your comment on the change.
postCommentForPullRequest
    :: Text -- ^ 'pcfprPullRequestId'
    -> Text -- ^ 'pcfprRepositoryName'
    -> Text -- ^ 'pcfprBeforeCommitId'
    -> Text -- ^ 'pcfprAfterCommitId'
    -> Text -- ^ 'pcfprContent'
    -> PostCommentForPullRequest
postCommentForPullRequest pPullRequestId_ pRepositoryName_ pBeforeCommitId_ pAfterCommitId_ pContent_ =
  PostCommentForPullRequest'
    { _pcfprLocation = Nothing
    , _pcfprClientRequestToken = Nothing
    , _pcfprPullRequestId = pPullRequestId_
    , _pcfprRepositoryName = pRepositoryName_
    , _pcfprBeforeCommitId = pBeforeCommitId_
    , _pcfprAfterCommitId = pAfterCommitId_
    , _pcfprContent = pContent_
    }


-- | The location of the change where you want to post your comment. If no location is provided, the comment will be posted as a general comment on the pull request difference between the before commit ID and the after commit ID.
pcfprLocation :: Lens' PostCommentForPullRequest (Maybe Location)
pcfprLocation = lens _pcfprLocation (\ s a -> s{_pcfprLocation = a})

-- | A unique, client-generated idempotency token that when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request will return information about the initial request that used that token.
pcfprClientRequestToken :: Lens' PostCommentForPullRequest (Maybe Text)
pcfprClientRequestToken = lens _pcfprClientRequestToken (\ s a -> s{_pcfprClientRequestToken = a})

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
pcfprPullRequestId :: Lens' PostCommentForPullRequest Text
pcfprPullRequestId = lens _pcfprPullRequestId (\ s a -> s{_pcfprPullRequestId = a})

-- | The name of the repository where you want to post a comment on a pull request.
pcfprRepositoryName :: Lens' PostCommentForPullRequest Text
pcfprRepositoryName = lens _pcfprRepositoryName (\ s a -> s{_pcfprRepositoryName = a})

-- | The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was created.
pcfprBeforeCommitId :: Lens' PostCommentForPullRequest Text
pcfprBeforeCommitId = lens _pcfprBeforeCommitId (\ s a -> s{_pcfprBeforeCommitId = a})

-- | The full commit ID of the commit in the source branch that is the current tip of the branch for the pull request when you post the comment.
pcfprAfterCommitId :: Lens' PostCommentForPullRequest Text
pcfprAfterCommitId = lens _pcfprAfterCommitId (\ s a -> s{_pcfprAfterCommitId = a})

-- | The content of your comment on the change.
pcfprContent :: Lens' PostCommentForPullRequest Text
pcfprContent = lens _pcfprContent (\ s a -> s{_pcfprContent = a})

instance AWSRequest PostCommentForPullRequest where
        type Rs PostCommentForPullRequest =
             PostCommentForPullRequestResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 PostCommentForPullRequestResponse' <$>
                   (x .?> "beforeBlobId") <*> (x .?> "location") <*>
                     (x .?> "afterCommitId")
                     <*> (x .?> "pullRequestId")
                     <*> (x .?> "afterBlobId")
                     <*> (x .?> "beforeCommitId")
                     <*> (x .?> "repositoryName")
                     <*> (x .?> "comment")
                     <*> (pure (fromEnum s)))

instance Hashable PostCommentForPullRequest where

instance NFData PostCommentForPullRequest where

instance ToHeaders PostCommentForPullRequest where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.PostCommentForPullRequest" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PostCommentForPullRequest where
        toJSON PostCommentForPullRequest'{..}
          = object
              (catMaybes
                 [("location" .=) <$> _pcfprLocation,
                  ("clientRequestToken" .=) <$>
                    _pcfprClientRequestToken,
                  Just ("pullRequestId" .= _pcfprPullRequestId),
                  Just ("repositoryName" .= _pcfprRepositoryName),
                  Just ("beforeCommitId" .= _pcfprBeforeCommitId),
                  Just ("afterCommitId" .= _pcfprAfterCommitId),
                  Just ("content" .= _pcfprContent)])

instance ToPath PostCommentForPullRequest where
        toPath = const "/"

instance ToQuery PostCommentForPullRequest where
        toQuery = const mempty

-- | /See:/ 'postCommentForPullRequestResponse' smart constructor.
data PostCommentForPullRequestResponse = PostCommentForPullRequestResponse'
  { _pcfprrsBeforeBlobId   :: !(Maybe Text)
  , _pcfprrsLocation       :: !(Maybe Location)
  , _pcfprrsAfterCommitId  :: !(Maybe Text)
  , _pcfprrsPullRequestId  :: !(Maybe Text)
  , _pcfprrsAfterBlobId    :: !(Maybe Text)
  , _pcfprrsBeforeCommitId :: !(Maybe Text)
  , _pcfprrsRepositoryName :: !(Maybe Text)
  , _pcfprrsComment        :: !(Maybe Comment)
  , _pcfprrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PostCommentForPullRequestResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcfprrsBeforeBlobId' - In the directionality of the pull request, the blob ID of the 'before' blob.
--
-- * 'pcfprrsLocation' - The location of the change where you posted your comment.
--
-- * 'pcfprrsAfterCommitId' - The full commit ID of the commit in the destination branch where the pull request will be merged.
--
-- * 'pcfprrsPullRequestId' - The system-generated ID of the pull request.
--
-- * 'pcfprrsAfterBlobId' - In the directionality of the pull request, the blob ID of the 'after' blob.
--
-- * 'pcfprrsBeforeCommitId' - The full commit ID of the commit in the source branch used to create the pull request, or in the case of an updated pull request, the full commit ID of the commit used to update the pull request.
--
-- * 'pcfprrsRepositoryName' - The name of the repository where you posted a comment on a pull request.
--
-- * 'pcfprrsComment' - The content of the comment you posted.
--
-- * 'pcfprrsResponseStatus' - -- | The response status code.
postCommentForPullRequestResponse
    :: Int -- ^ 'pcfprrsResponseStatus'
    -> PostCommentForPullRequestResponse
postCommentForPullRequestResponse pResponseStatus_ =
  PostCommentForPullRequestResponse'
    { _pcfprrsBeforeBlobId = Nothing
    , _pcfprrsLocation = Nothing
    , _pcfprrsAfterCommitId = Nothing
    , _pcfprrsPullRequestId = Nothing
    , _pcfprrsAfterBlobId = Nothing
    , _pcfprrsBeforeCommitId = Nothing
    , _pcfprrsRepositoryName = Nothing
    , _pcfprrsComment = Nothing
    , _pcfprrsResponseStatus = pResponseStatus_
    }


-- | In the directionality of the pull request, the blob ID of the 'before' blob.
pcfprrsBeforeBlobId :: Lens' PostCommentForPullRequestResponse (Maybe Text)
pcfprrsBeforeBlobId = lens _pcfprrsBeforeBlobId (\ s a -> s{_pcfprrsBeforeBlobId = a})

-- | The location of the change where you posted your comment.
pcfprrsLocation :: Lens' PostCommentForPullRequestResponse (Maybe Location)
pcfprrsLocation = lens _pcfprrsLocation (\ s a -> s{_pcfprrsLocation = a})

-- | The full commit ID of the commit in the destination branch where the pull request will be merged.
pcfprrsAfterCommitId :: Lens' PostCommentForPullRequestResponse (Maybe Text)
pcfprrsAfterCommitId = lens _pcfprrsAfterCommitId (\ s a -> s{_pcfprrsAfterCommitId = a})

-- | The system-generated ID of the pull request.
pcfprrsPullRequestId :: Lens' PostCommentForPullRequestResponse (Maybe Text)
pcfprrsPullRequestId = lens _pcfprrsPullRequestId (\ s a -> s{_pcfprrsPullRequestId = a})

-- | In the directionality of the pull request, the blob ID of the 'after' blob.
pcfprrsAfterBlobId :: Lens' PostCommentForPullRequestResponse (Maybe Text)
pcfprrsAfterBlobId = lens _pcfprrsAfterBlobId (\ s a -> s{_pcfprrsAfterBlobId = a})

-- | The full commit ID of the commit in the source branch used to create the pull request, or in the case of an updated pull request, the full commit ID of the commit used to update the pull request.
pcfprrsBeforeCommitId :: Lens' PostCommentForPullRequestResponse (Maybe Text)
pcfprrsBeforeCommitId = lens _pcfprrsBeforeCommitId (\ s a -> s{_pcfprrsBeforeCommitId = a})

-- | The name of the repository where you posted a comment on a pull request.
pcfprrsRepositoryName :: Lens' PostCommentForPullRequestResponse (Maybe Text)
pcfprrsRepositoryName = lens _pcfprrsRepositoryName (\ s a -> s{_pcfprrsRepositoryName = a})

-- | The content of the comment you posted.
pcfprrsComment :: Lens' PostCommentForPullRequestResponse (Maybe Comment)
pcfprrsComment = lens _pcfprrsComment (\ s a -> s{_pcfprrsComment = a})

-- | -- | The response status code.
pcfprrsResponseStatus :: Lens' PostCommentForPullRequestResponse Int
pcfprrsResponseStatus = lens _pcfprrsResponseStatus (\ s a -> s{_pcfprrsResponseStatus = a})

instance NFData PostCommentForPullRequestResponse
         where
