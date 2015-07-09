{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
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
    , dtacrsrStatus
    , dtacrsrStatuses
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
                   <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "statuses" .!@ mempty))

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
-- * 'dtacrsrStatus'
--
-- * 'dtacrsrStatuses'
data DescribeTrustedAdvisorCheckRefreshStatusesResponse = DescribeTrustedAdvisorCheckRefreshStatusesResponse'
    { _dtacrsrStatus   :: !Int
    , _dtacrsrStatuses :: ![TrustedAdvisorCheckRefreshStatus]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTrustedAdvisorCheckRefreshStatusesResponse' smart constructor.
describeTrustedAdvisorCheckRefreshStatusesResponse :: Int -> DescribeTrustedAdvisorCheckRefreshStatusesResponse
describeTrustedAdvisorCheckRefreshStatusesResponse pStatus =
    DescribeTrustedAdvisorCheckRefreshStatusesResponse'
    { _dtacrsrStatus = pStatus
    , _dtacrsrStatuses = mempty
    }

-- | FIXME: Undocumented member.
dtacrsrStatus :: Lens' DescribeTrustedAdvisorCheckRefreshStatusesResponse Int
dtacrsrStatus = lens _dtacrsrStatus (\ s a -> s{_dtacrsrStatus = a});

-- | The refresh status of the specified Trusted Advisor checks.
dtacrsrStatuses :: Lens' DescribeTrustedAdvisorCheckRefreshStatusesResponse [TrustedAdvisorCheckRefreshStatus]
dtacrsrStatuses = lens _dtacrsrStatuses (\ s a -> s{_dtacrsrStatuses = a});
