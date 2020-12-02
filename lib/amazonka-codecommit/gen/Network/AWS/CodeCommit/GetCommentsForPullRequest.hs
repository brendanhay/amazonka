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
-- Module      : Network.AWS.CodeCommit.GetCommentsForPullRequest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns comments made on a pull request.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.GetCommentsForPullRequest
    (
    -- * Creating a Request
      getCommentsForPullRequest
    , GetCommentsForPullRequest
    -- * Request Lenses
    , gcfprAfterCommitId
    , gcfprNextToken
    , gcfprBeforeCommitId
    , gcfprRepositoryName
    , gcfprMaxResults
    , gcfprPullRequestId

    -- * Destructuring the Response
    , getCommentsForPullRequestResponse
    , GetCommentsForPullRequestResponse
    -- * Response Lenses
    , gcfprrsCommentsForPullRequestData
    , gcfprrsNextToken
    , gcfprrsResponseStatus
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCommentsForPullRequest' smart constructor.
data GetCommentsForPullRequest = GetCommentsForPullRequest'
  { _gcfprAfterCommitId  :: !(Maybe Text)
  , _gcfprNextToken      :: !(Maybe Text)
  , _gcfprBeforeCommitId :: !(Maybe Text)
  , _gcfprRepositoryName :: !(Maybe Text)
  , _gcfprMaxResults     :: !(Maybe Int)
  , _gcfprPullRequestId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCommentsForPullRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcfprAfterCommitId' - The full commit ID of the commit in the source branch that was the tip of the branch at the time the comment was made.
--
-- * 'gcfprNextToken' - An enumeration token that when provided in a request, returns the next batch of the results.
--
-- * 'gcfprBeforeCommitId' - The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was created.
--
-- * 'gcfprRepositoryName' - The name of the repository that contains the pull request.
--
-- * 'gcfprMaxResults' - A non-negative integer used to limit the number of returned results. The default is 100 comments. You can return up to 500 comments with a single request.
--
-- * 'gcfprPullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
getCommentsForPullRequest
    :: Text -- ^ 'gcfprPullRequestId'
    -> GetCommentsForPullRequest
getCommentsForPullRequest pPullRequestId_ =
  GetCommentsForPullRequest'
    { _gcfprAfterCommitId = Nothing
    , _gcfprNextToken = Nothing
    , _gcfprBeforeCommitId = Nothing
    , _gcfprRepositoryName = Nothing
    , _gcfprMaxResults = Nothing
    , _gcfprPullRequestId = pPullRequestId_
    }


-- | The full commit ID of the commit in the source branch that was the tip of the branch at the time the comment was made.
gcfprAfterCommitId :: Lens' GetCommentsForPullRequest (Maybe Text)
gcfprAfterCommitId = lens _gcfprAfterCommitId (\ s a -> s{_gcfprAfterCommitId = a})

-- | An enumeration token that when provided in a request, returns the next batch of the results.
gcfprNextToken :: Lens' GetCommentsForPullRequest (Maybe Text)
gcfprNextToken = lens _gcfprNextToken (\ s a -> s{_gcfprNextToken = a})

-- | The full commit ID of the commit in the destination branch that was the tip of the branch at the time the pull request was created.
gcfprBeforeCommitId :: Lens' GetCommentsForPullRequest (Maybe Text)
gcfprBeforeCommitId = lens _gcfprBeforeCommitId (\ s a -> s{_gcfprBeforeCommitId = a})

-- | The name of the repository that contains the pull request.
gcfprRepositoryName :: Lens' GetCommentsForPullRequest (Maybe Text)
gcfprRepositoryName = lens _gcfprRepositoryName (\ s a -> s{_gcfprRepositoryName = a})

-- | A non-negative integer used to limit the number of returned results. The default is 100 comments. You can return up to 500 comments with a single request.
gcfprMaxResults :: Lens' GetCommentsForPullRequest (Maybe Int)
gcfprMaxResults = lens _gcfprMaxResults (\ s a -> s{_gcfprMaxResults = a})

-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
gcfprPullRequestId :: Lens' GetCommentsForPullRequest Text
gcfprPullRequestId = lens _gcfprPullRequestId (\ s a -> s{_gcfprPullRequestId = a})

instance AWSPager GetCommentsForPullRequest where
        page rq rs
          | stop (rs ^. gcfprrsNextToken) = Nothing
          | stop (rs ^. gcfprrsCommentsForPullRequestData) =
            Nothing
          | otherwise =
            Just $ rq & gcfprNextToken .~ rs ^. gcfprrsNextToken

instance AWSRequest GetCommentsForPullRequest where
        type Rs GetCommentsForPullRequest =
             GetCommentsForPullRequestResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 GetCommentsForPullRequestResponse' <$>
                   (x .?> "commentsForPullRequestData" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetCommentsForPullRequest where

instance NFData GetCommentsForPullRequest where

instance ToHeaders GetCommentsForPullRequest where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.GetCommentsForPullRequest" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetCommentsForPullRequest where
        toJSON GetCommentsForPullRequest'{..}
          = object
              (catMaybes
                 [("afterCommitId" .=) <$> _gcfprAfterCommitId,
                  ("nextToken" .=) <$> _gcfprNextToken,
                  ("beforeCommitId" .=) <$> _gcfprBeforeCommitId,
                  ("repositoryName" .=) <$> _gcfprRepositoryName,
                  ("maxResults" .=) <$> _gcfprMaxResults,
                  Just ("pullRequestId" .= _gcfprPullRequestId)])

instance ToPath GetCommentsForPullRequest where
        toPath = const "/"

instance ToQuery GetCommentsForPullRequest where
        toQuery = const mempty

-- | /See:/ 'getCommentsForPullRequestResponse' smart constructor.
data GetCommentsForPullRequestResponse = GetCommentsForPullRequestResponse'
  { _gcfprrsCommentsForPullRequestData :: !(Maybe [CommentsForPullRequest])
  , _gcfprrsNextToken                  :: !(Maybe Text)
  , _gcfprrsResponseStatus             :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCommentsForPullRequestResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcfprrsCommentsForPullRequestData' - An array of comment objects on the pull request.
--
-- * 'gcfprrsNextToken' - An enumeration token that can be used in a request to return the next batch of the results.
--
-- * 'gcfprrsResponseStatus' - -- | The response status code.
getCommentsForPullRequestResponse
    :: Int -- ^ 'gcfprrsResponseStatus'
    -> GetCommentsForPullRequestResponse
getCommentsForPullRequestResponse pResponseStatus_ =
  GetCommentsForPullRequestResponse'
    { _gcfprrsCommentsForPullRequestData = Nothing
    , _gcfprrsNextToken = Nothing
    , _gcfprrsResponseStatus = pResponseStatus_
    }


-- | An array of comment objects on the pull request.
gcfprrsCommentsForPullRequestData :: Lens' GetCommentsForPullRequestResponse [CommentsForPullRequest]
gcfprrsCommentsForPullRequestData = lens _gcfprrsCommentsForPullRequestData (\ s a -> s{_gcfprrsCommentsForPullRequestData = a}) . _Default . _Coerce

-- | An enumeration token that can be used in a request to return the next batch of the results.
gcfprrsNextToken :: Lens' GetCommentsForPullRequestResponse (Maybe Text)
gcfprrsNextToken = lens _gcfprrsNextToken (\ s a -> s{_gcfprrsNextToken = a})

-- | -- | The response status code.
gcfprrsResponseStatus :: Lens' GetCommentsForPullRequestResponse Int
gcfprrsResponseStatus = lens _gcfprrsResponseStatus (\ s a -> s{_gcfprrsResponseStatus = a})

instance NFData GetCommentsForPullRequestResponse
         where
