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
-- Module      : Network.AWS.CodeCommit.GetPullRequest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a pull request in a specified repository.
--
--
module Network.AWS.CodeCommit.GetPullRequest
    (
    -- * Creating a Request
      getPullRequest
    , GetPullRequest
    -- * Request Lenses
    , gprPullRequestId

    -- * Destructuring the Response
    , getPullRequestResponse
    , GetPullRequestResponse
    -- * Response Lenses
    , gprrsResponseStatus
    , gprrsPullRequest
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPullRequest' smart constructor.
newtype GetPullRequest = GetPullRequest'
  { _gprPullRequestId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPullRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprPullRequestId' - The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
getPullRequest
    :: Text -- ^ 'gprPullRequestId'
    -> GetPullRequest
getPullRequest pPullRequestId_ =
  GetPullRequest' {_gprPullRequestId = pPullRequestId_}


-- | The system-generated ID of the pull request. To get this ID, use 'ListPullRequests' .
gprPullRequestId :: Lens' GetPullRequest Text
gprPullRequestId = lens _gprPullRequestId (\ s a -> s{_gprPullRequestId = a})

instance AWSRequest GetPullRequest where
        type Rs GetPullRequest = GetPullRequestResponse
        request = postJSON codeCommit
        response
          = receiveJSON
              (\ s h x ->
                 GetPullRequestResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "pullRequest"))

instance Hashable GetPullRequest where

instance NFData GetPullRequest where

instance ToHeaders GetPullRequest where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeCommit_20150413.GetPullRequest" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetPullRequest where
        toJSON GetPullRequest'{..}
          = object
              (catMaybes
                 [Just ("pullRequestId" .= _gprPullRequestId)])

instance ToPath GetPullRequest where
        toPath = const "/"

instance ToQuery GetPullRequest where
        toQuery = const mempty

-- | /See:/ 'getPullRequestResponse' smart constructor.
data GetPullRequestResponse = GetPullRequestResponse'
  { _gprrsResponseStatus :: !Int
  , _gprrsPullRequest    :: !PullRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPullRequestResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprrsResponseStatus' - -- | The response status code.
--
-- * 'gprrsPullRequest' - Information about the specified pull request.
getPullRequestResponse
    :: Int -- ^ 'gprrsResponseStatus'
    -> PullRequest -- ^ 'gprrsPullRequest'
    -> GetPullRequestResponse
getPullRequestResponse pResponseStatus_ pPullRequest_ =
  GetPullRequestResponse'
    {_gprrsResponseStatus = pResponseStatus_, _gprrsPullRequest = pPullRequest_}


-- | -- | The response status code.
gprrsResponseStatus :: Lens' GetPullRequestResponse Int
gprrsResponseStatus = lens _gprrsResponseStatus (\ s a -> s{_gprrsResponseStatus = a})

-- | Information about the specified pull request.
gprrsPullRequest :: Lens' GetPullRequestResponse PullRequest
gprrsPullRequest = lens _gprrsPullRequest (\ s a -> s{_gprrsPullRequest = a})

instance NFData GetPullRequestResponse where
