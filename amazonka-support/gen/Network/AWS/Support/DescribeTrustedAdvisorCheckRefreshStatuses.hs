{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses
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

-- | Returns the refresh status of the Trusted Advisor checks that have the
-- specified check IDs. Check IDs can be obtained by calling
-- DescribeTrustedAdvisorChecks.
--
-- <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_DescribeTrustedAdvisorCheckRefreshStatuses.html>
module Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses
    (
    -- * Request
      DescribeTrustedAdvisorCheckRefreshStatuses
    -- ** Request constructor
    , describeTrustedAdvisorCheckRefreshStatuses
    -- ** Request lenses
    , dtacrsCheckIds

    -- * Response
    , DescribeTrustedAdvisorCheckRefreshStatusesResponse
    -- ** Response constructor
    , describeTrustedAdvisorCheckRefreshStatusesResponse
    -- ** Response lenses
    , dtacrsrStatuses
    , dtacrsrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Support.Types

-- | /See:/ 'describeTrustedAdvisorCheckRefreshStatuses' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtacrsCheckIds'
newtype DescribeTrustedAdvisorCheckRefreshStatuses = DescribeTrustedAdvisorCheckRefreshStatuses'
    { _dtacrsCheckIds :: [Text]
    } deriving (Eq,Read,Show)

-- | 'DescribeTrustedAdvisorCheckRefreshStatuses' smart constructor.
describeTrustedAdvisorCheckRefreshStatuses :: DescribeTrustedAdvisorCheckRefreshStatuses
describeTrustedAdvisorCheckRefreshStatuses =
    DescribeTrustedAdvisorCheckRefreshStatuses'
    { _dtacrsCheckIds = mempty
    }

-- | The IDs of the Trusted Advisor checks.
dtacrsCheckIds :: Lens' DescribeTrustedAdvisorCheckRefreshStatuses [Text]
dtacrsCheckIds = lens _dtacrsCheckIds (\ s a -> s{_dtacrsCheckIds = a});

instance AWSRequest
         DescribeTrustedAdvisorCheckRefreshStatuses where
        type Sv DescribeTrustedAdvisorCheckRefreshStatuses =
             Support
        type Rs DescribeTrustedAdvisorCheckRefreshStatuses =
             DescribeTrustedAdvisorCheckRefreshStatusesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTrustedAdvisorCheckRefreshStatusesResponse'
                   <$> (x .?> "statuses" .!@ mempty) <*> (pure s))

instance ToHeaders
         DescribeTrustedAdvisorCheckRefreshStatuses where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSSupport_20130415.DescribeTrustedAdvisorCheckRefreshStatuses"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
         DescribeTrustedAdvisorCheckRefreshStatuses where
        toJSON
          DescribeTrustedAdvisorCheckRefreshStatuses'{..}
          = object ["checkIds" .= _dtacrsCheckIds]

instance ToPath
         DescribeTrustedAdvisorCheckRefreshStatuses where
        toPath = const "/"

instance ToQuery
         DescribeTrustedAdvisorCheckRefreshStatuses where
        toQuery = const mempty

-- | The statuses of the Trusted Advisor checks returned by the
-- DescribeTrustedAdvisorCheckRefreshStatuses operation.
--
-- /See:/ 'describeTrustedAdvisorCheckRefreshStatusesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtacrsrStatuses'
--
-- * 'dtacrsrStatus'
data DescribeTrustedAdvisorCheckRefreshStatusesResponse = DescribeTrustedAdvisorCheckRefreshStatusesResponse'
    { _dtacrsrStatuses :: ![TrustedAdvisorCheckRefreshStatus]
    , _dtacrsrStatus   :: !Status
    } deriving (Eq,Show)

-- | 'DescribeTrustedAdvisorCheckRefreshStatusesResponse' smart constructor.
describeTrustedAdvisorCheckRefreshStatusesResponse :: Status -> DescribeTrustedAdvisorCheckRefreshStatusesResponse
describeTrustedAdvisorCheckRefreshStatusesResponse pStatus =
    DescribeTrustedAdvisorCheckRefreshStatusesResponse'
    { _dtacrsrStatuses = mempty
    , _dtacrsrStatus = pStatus
    }

-- | The refresh status of the specified Trusted Advisor checks.
dtacrsrStatuses :: Lens' DescribeTrustedAdvisorCheckRefreshStatusesResponse [TrustedAdvisorCheckRefreshStatus]
dtacrsrStatuses = lens _dtacrsrStatuses (\ s a -> s{_dtacrsrStatuses = a});

-- | FIXME: Undocumented member.
dtacrsrStatus :: Lens' DescribeTrustedAdvisorCheckRefreshStatusesResponse Status
dtacrsrStatus = lens _dtacrsrStatus (\ s a -> s{_dtacrsrStatus = a});
