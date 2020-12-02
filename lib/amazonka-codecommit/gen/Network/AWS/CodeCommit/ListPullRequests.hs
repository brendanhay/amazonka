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
-- Module      : Network.AWS.CodeCommit.ListPullRequests
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of pull requests for a specified repository. The return list can be refined by pull request status or pull request author ARN.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.ListPullRequests
    (
    -- * Creating a Request
      listPullRequests
    , ListPullRequests
    -- * Request Lenses
    , lprAuthorARN
    , lprNextToken
    , lprPullRequestStatus
    , lprMaxResults
    , lprRepositoryName

    -- * Destructuring the Response
    , listPullRequestsResponse
    , ListPullRequestsResponse
    -- * Response Lenses
    , lprrsNextToken
    , lprrsResponseStatus
    , lprrsPullRequestIds
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPullRequests' smart constructor.
data ListPullRequests = ListPullRequests'
  { _lprAuthorARN         :: !(Maybe Text)
  , _lprNextToken         :: !(Maybe Text)
  , _lprPullRequestStatus :: !(Maybe PullRequestStatusEnum)
  , _lprMaxResults        :: !(Maybe Int)
  , _lprRepositoryName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPullRequests' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprAuthorARN' - Optional. The Amazon Resource Name (ARN) of the user who created the pull request. If used, this filters the results to pull requests created by that user.
--
-- * 'lprNextToken' - An enumeration token that when provided in a request, returns the next batch of the results.
--
-- * 'lprPullRequestStatus' - Optional. The status of the pull request. If used, this refines the results to the pull requests that match the specified status.
--
-- * 'lprMaxResults' - A non-negative integer used to limit the number of returned results.
--
-- * 'lprRepositoryName' - The name of the repository for which you want to list pull requests.
listPullRequests
    :: Text -- ^ 'lprRepositoryName'
    -> ListPullRequests
listPullRequests pRepositoryName_ =
  ListPullRequests'
    { _lprAuthorARN = Nothing
    , _lprNextToken = Nothing
    , _lprPullRequestStatus = Nothing
    , _lprMaxResults = Nothing
    , _lprRepositoryName = pRepositoryName_
    }


-- | Optional. The Amazon Resource Name (ARN) of the user who created the pull request. If used, this filters the results to pull requests created by that user.
lprAuthorARN :: Lens' ListPullRequests (Maybe Text)
lprAuthorARN = lens _lprAuthorARN (\ s a -> s{_lprAuthorARN = a})

-- | An enumeration token that when provided in a request, returns the next batch of the results.
lprNextToken :: Lens' ListPullRequests (Maybe Text)
lprNextToken = lens _lprNextToken (\ s a -> s{_lprNextToken = a})

-- | Optional. The status of the pull request. If used, this refines the results to the pull requests that match the specified status.
lprPullRequestStatus :: Lens' ListPullRequests (Maybe PullRequestStatusEnum)
lprPullRequestStatus = lens _lprPullRequestStatus (\ s a -> s{_lprPullRequestStatus = a})

-- | A non-negative integer used to limit the number of returned results.
lprMaxResults :: Lens' ListPullRequests (Maybe Int)
lprMaxResults = lens _lprMaxResults (\ s a -> s{_lprMaxResults = a})

-- | The name of the repository for which you want to list pull requests.
lprRepositoryName :: Lens' ListPullRequests Text
lprRepositoryName = lens _lprRepositoryName (\ s a -> s{_lprRepositoryName = a})

instance AWSPager ListPullRequests where
        page rq rs
          | stop (rs ^. lprrsNextToken) = Nothing
          | stop (rs ^. lprrsPullRequestIds) = Nothing
          | otherwise =
            Just $ rq & lprNextToken .~ rs ^. lprrsNextToken

instance AWSRequest ListPullRequests where
        type Rs ListPullRequests = ListPullRequestsResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 ListPullRequestsResponse' <$>
                   (x .?> "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "pullRequestIds" .!@ mempty))

instance Hashable ListPullRequests where

instance NFData ListPullRequests where

instance ToHeaders ListPullRequests where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.ListPullRequests" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListPullRequests where
        toJSON ListPullRequests'{..}
          = object
              (catMaybes
                 [("authorArn" .=) <$> _lprAuthorARN,
                  ("nextToken" .=) <$> _lprNextToken,
                  ("pullRequestStatus" .=) <$> _lprPullRequestStatus,
                  ("maxResults" .=) <$> _lprMaxResults,
                  Just ("repositoryName" .= _lprRepositoryName)])

instance ToPath ListPullRequests where
        toPath = const "/"

instance ToQuery ListPullRequests where
        toQuery = const mempty

-- | /See:/ 'listPullRequestsResponse' smart constructor.
data ListPullRequestsResponse = ListPullRequestsResponse'
  { _lprrsNextToken      :: !(Maybe Text)
  , _lprrsResponseStatus :: !Int
  , _lprrsPullRequestIds :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPullRequestsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprrsNextToken' - An enumeration token that when provided in a request, returns the next batch of the results.
--
-- * 'lprrsResponseStatus' - -- | The response status code.
--
-- * 'lprrsPullRequestIds' - The system-generated IDs of the pull requests.
listPullRequestsResponse
    :: Int -- ^ 'lprrsResponseStatus'
    -> ListPullRequestsResponse
listPullRequestsResponse pResponseStatus_ =
  ListPullRequestsResponse'
    { _lprrsNextToken = Nothing
    , _lprrsResponseStatus = pResponseStatus_
    , _lprrsPullRequestIds = mempty
    }


-- | An enumeration token that when provided in a request, returns the next batch of the results.
lprrsNextToken :: Lens' ListPullRequestsResponse (Maybe Text)
lprrsNextToken = lens _lprrsNextToken (\ s a -> s{_lprrsNextToken = a})

-- | -- | The response status code.
lprrsResponseStatus :: Lens' ListPullRequestsResponse Int
lprrsResponseStatus = lens _lprrsResponseStatus (\ s a -> s{_lprrsResponseStatus = a})

-- | The system-generated IDs of the pull requests.
lprrsPullRequestIds :: Lens' ListPullRequestsResponse [Text]
lprrsPullRequestIds = lens _lprrsPullRequestIds (\ s a -> s{_lprrsPullRequestIds = a}) . _Coerce

instance NFData ListPullRequestsResponse where
