{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckSummaries
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the summaries of the results of the Trusted Advisor checks that
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
    , dtacsrsStatus
    , dtacsrsSummaries
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTrustedAdvisorCheckSummaries' smart constructor.
describeTrustedAdvisorCheckSummaries :: DescribeTrustedAdvisorCheckSummaries
describeTrustedAdvisorCheckSummaries =
    DescribeTrustedAdvisorCheckSummaries'
    { _dtacsCheckIds = mempty
    }

-- | The IDs of the Trusted Advisor checks.
dtacsCheckIds :: Lens' DescribeTrustedAdvisorCheckSummaries [Text]
dtacsCheckIds = lens _dtacsCheckIds (\ s a -> s{_dtacsCheckIds = a}) . _Coerce;

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
                   (pure (fromEnum s)) <*>
                     (x .?> "summaries" .!@ mempty))

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
        toPath = const mempty

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
-- * 'dtacsrsStatus'
--
-- * 'dtacsrsSummaries'
data DescribeTrustedAdvisorCheckSummariesResponse = DescribeTrustedAdvisorCheckSummariesResponse'
    { _dtacsrsStatus    :: !Int
    , _dtacsrsSummaries :: ![TrustedAdvisorCheckSummary]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTrustedAdvisorCheckSummariesResponse' smart constructor.
describeTrustedAdvisorCheckSummariesResponse :: Int -> DescribeTrustedAdvisorCheckSummariesResponse
describeTrustedAdvisorCheckSummariesResponse pStatus_ =
    DescribeTrustedAdvisorCheckSummariesResponse'
    { _dtacsrsStatus = pStatus_
    , _dtacsrsSummaries = mempty
    }

-- | FIXME: Undocumented member.
dtacsrsStatus :: Lens' DescribeTrustedAdvisorCheckSummariesResponse Int
dtacsrsStatus = lens _dtacsrsStatus (\ s a -> s{_dtacsrsStatus = a});

-- | The summary information for the requested Trusted Advisor checks.
dtacsrsSummaries :: Lens' DescribeTrustedAdvisorCheckSummariesResponse [TrustedAdvisorCheckSummary]
dtacsrsSummaries = lens _dtacsrsSummaries (\ s a -> s{_dtacsrsSummaries = a}) . _Coerce;
