{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Support.RefreshTrustedAdvisorCheck
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Requests a refresh of the Trusted Advisor check that has the specified
-- check ID. Check IDs can be obtained by calling
-- DescribeTrustedAdvisorChecks.
--
-- The response contains a TrustedAdvisorCheckRefreshStatus object, which
-- contains these fields:
--
-- -   __Status.__ The refresh status of the check: \"none\", \"enqueued\",
--     \"processing\", \"success\", or \"abandoned\".
-- -   __MillisUntilNextRefreshable.__ The amount of time, in milliseconds,
--     until the check is eligible for refresh.
-- -   __CheckId.__ The unique identifier for the check.
--
-- <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_RefreshTrustedAdvisorCheck.html>
module Network.AWS.Support.RefreshTrustedAdvisorCheck
    (
    -- * Request
      RefreshTrustedAdvisorCheck
    -- ** Request constructor
    , refreshTrustedAdvisorCheck
    -- ** Request lenses
    , rtacCheckId

    -- * Response
    , RefreshTrustedAdvisorCheckResponse
    -- ** Response constructor
    , refreshTrustedAdvisorCheckResponse
    -- ** Response lenses
    , rtacrStatus
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Support.Types

-- | /See:/ 'refreshTrustedAdvisorCheck' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtacCheckId'
newtype RefreshTrustedAdvisorCheck = RefreshTrustedAdvisorCheck'{_rtacCheckId :: Text} deriving (Eq, Read, Show)

-- | 'RefreshTrustedAdvisorCheck' smart constructor.
refreshTrustedAdvisorCheck :: Text -> RefreshTrustedAdvisorCheck
refreshTrustedAdvisorCheck pCheckId = RefreshTrustedAdvisorCheck'{_rtacCheckId = pCheckId};

-- | The unique identifier for the Trusted Advisor check.
rtacCheckId :: Lens' RefreshTrustedAdvisorCheck Text
rtacCheckId = lens _rtacCheckId (\ s a -> s{_rtacCheckId = a});

instance AWSRequest RefreshTrustedAdvisorCheck where
        type Sv RefreshTrustedAdvisorCheck = Support
        type Rs RefreshTrustedAdvisorCheck =
             RefreshTrustedAdvisorCheckResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RefreshTrustedAdvisorCheckResponse' <$>
                   (x .:> "status"))

instance ToHeaders RefreshTrustedAdvisorCheck where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSSupport_20130415.RefreshTrustedAdvisorCheck" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RefreshTrustedAdvisorCheck where
        toJSON RefreshTrustedAdvisorCheck'{..}
          = object ["checkId" .= _rtacCheckId]

instance ToPath RefreshTrustedAdvisorCheck where
        toPath = const "/"

instance ToQuery RefreshTrustedAdvisorCheck where
        toQuery = const mempty

-- | /See:/ 'refreshTrustedAdvisorCheckResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtacrStatus'
newtype RefreshTrustedAdvisorCheckResponse = RefreshTrustedAdvisorCheckResponse'{_rtacrStatus :: TrustedAdvisorCheckRefreshStatus} deriving (Eq, Read, Show)

-- | 'RefreshTrustedAdvisorCheckResponse' smart constructor.
refreshTrustedAdvisorCheckResponse :: TrustedAdvisorCheckRefreshStatus -> RefreshTrustedAdvisorCheckResponse
refreshTrustedAdvisorCheckResponse pStatus = RefreshTrustedAdvisorCheckResponse'{_rtacrStatus = pStatus};

-- | The current refresh status for a check, including the amount of time
-- until the check is eligible for refresh.
rtacrStatus :: Lens' RefreshTrustedAdvisorCheckResponse TrustedAdvisorCheckRefreshStatus
rtacrStatus = lens _rtacrStatus (\ s a -> s{_rtacrStatus = a});
