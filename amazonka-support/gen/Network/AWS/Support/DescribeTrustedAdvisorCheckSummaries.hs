{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckSummaries
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

-- | Returns the summaries of the results of the Trusted Advisor checks that
-- have the specified check IDs. Check IDs can be obtained by calling
-- DescribeTrustedAdvisorChecks.
--
-- The response contains an array of TrustedAdvisorCheckSummary objects.
--
-- <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_DescribeTrustedAdvisorCheckSummaries.html>
module Network.AWS.Support.DescribeTrustedAdvisorCheckSummaries
    (
    -- * Request
      DescribeTrustedAdvisorCheckSummaries
    -- ** Request constructor
    , describeTrustedAdvisorCheckSummaries
    -- ** Request lenses
    , dtacsCheckIds

    -- * Response
    , DescribeTrustedAdvisorCheckSummariesResponse
    -- ** Response constructor
    , describeTrustedAdvisorCheckSummariesResponse
    -- ** Response lenses
    , dtacsrSummaries
    , dtacsrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Support.Types

-- | /See:/ 'describeTrustedAdvisorCheckSummaries' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtacsCheckIds'
newtype DescribeTrustedAdvisorCheckSummaries = DescribeTrustedAdvisorCheckSummaries'
    { _dtacsCheckIds :: [Text]
    } deriving (Eq,Read,Show)

-- | 'DescribeTrustedAdvisorCheckSummaries' smart constructor.
describeTrustedAdvisorCheckSummaries :: DescribeTrustedAdvisorCheckSummaries
describeTrustedAdvisorCheckSummaries =
    DescribeTrustedAdvisorCheckSummaries'
    { _dtacsCheckIds = mempty
    }

-- | The IDs of the Trusted Advisor checks.
dtacsCheckIds :: Lens' DescribeTrustedAdvisorCheckSummaries [Text]
dtacsCheckIds = lens _dtacsCheckIds (\ s a -> s{_dtacsCheckIds = a});

instance AWSRequest
         DescribeTrustedAdvisorCheckSummaries where
        type Sv DescribeTrustedAdvisorCheckSummaries =
             Support
        type Rs DescribeTrustedAdvisorCheckSummaries =
             DescribeTrustedAdvisorCheckSummariesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTrustedAdvisorCheckSummariesResponse' <$>
                   (x .?> "summaries" .!@ mempty) <*> (pure s))

instance ToHeaders
         DescribeTrustedAdvisorCheckSummaries where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSSupport_20130415.DescribeTrustedAdvisorCheckSummaries"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTrustedAdvisorCheckSummaries
         where
        toJSON DescribeTrustedAdvisorCheckSummaries'{..}
          = object ["checkIds" .= _dtacsCheckIds]

instance ToPath DescribeTrustedAdvisorCheckSummaries
         where
        toPath = const "/"

instance ToQuery DescribeTrustedAdvisorCheckSummaries
         where
        toQuery = const mempty

-- | The summaries of the Trusted Advisor checks returned by the
-- DescribeTrustedAdvisorCheckSummaries operation.
--
-- /See:/ 'describeTrustedAdvisorCheckSummariesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtacsrSummaries'
--
-- * 'dtacsrStatus'
data DescribeTrustedAdvisorCheckSummariesResponse = DescribeTrustedAdvisorCheckSummariesResponse'
    { _dtacsrSummaries :: ![TrustedAdvisorCheckSummary]
    , _dtacsrStatus    :: !Status
    } deriving (Eq,Read,Show)

-- | 'DescribeTrustedAdvisorCheckSummariesResponse' smart constructor.
describeTrustedAdvisorCheckSummariesResponse :: Status -> DescribeTrustedAdvisorCheckSummariesResponse
describeTrustedAdvisorCheckSummariesResponse pStatus =
    DescribeTrustedAdvisorCheckSummariesResponse'
    { _dtacsrSummaries = mempty
    , _dtacsrStatus = pStatus
    }

-- | The summary information for the requested Trusted Advisor checks.
dtacsrSummaries :: Lens' DescribeTrustedAdvisorCheckSummariesResponse [TrustedAdvisorCheckSummary]
dtacsrSummaries = lens _dtacsrSummaries (\ s a -> s{_dtacsrSummaries = a});

-- | FIXME: Undocumented member.
dtacsrStatus :: Lens' DescribeTrustedAdvisorCheckSummariesResponse Status
dtacsrStatus = lens _dtacsrStatus (\ s a -> s{_dtacsrStatus = a});
