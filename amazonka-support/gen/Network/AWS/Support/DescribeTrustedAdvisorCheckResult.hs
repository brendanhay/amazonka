{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckResult
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

-- | Returns the results of the Trusted Advisor check that has the specified
-- check ID. Check IDs can be obtained by calling
-- DescribeTrustedAdvisorChecks.
--
-- The response contains a TrustedAdvisorCheckResult object, which contains
-- these three objects:
--
-- -   TrustedAdvisorCategorySpecificSummary
-- -   TrustedAdvisorResourceDetail
-- -   TrustedAdvisorResourcesSummary
--
-- In addition, the response contains these fields:
--
-- -   __Status.__ The alert status of the check: \"ok\" (green),
--     \"warning\" (yellow), \"error\" (red), or \"not_available\".
-- -   __Timestamp.__ The time of the last refresh of the check.
-- -   __CheckId.__ The unique identifier for the check.
--
-- <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_DescribeTrustedAdvisorCheckResult.html>
module Network.AWS.Support.DescribeTrustedAdvisorCheckResult
    (
    -- * Request
      DescribeTrustedAdvisorCheckResult
    -- ** Request constructor
    , describeTrustedAdvisorCheckResult
    -- ** Request lenses
    , dtacrLanguage
    , dtacrCheckId

    -- * Response
    , DescribeTrustedAdvisorCheckResultResponse
    -- ** Response constructor
    , describeTrustedAdvisorCheckResultResponse
    -- ** Response lenses
    , dtacrrResult
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Support.Types

-- | /See:/ 'describeTrustedAdvisorCheckResult' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtacrLanguage'
--
-- * 'dtacrCheckId'
data DescribeTrustedAdvisorCheckResult = DescribeTrustedAdvisorCheckResult'{_dtacrLanguage :: Maybe Text, _dtacrCheckId :: Text} deriving (Eq, Read, Show)

-- | 'DescribeTrustedAdvisorCheckResult' smart constructor.
describeTrustedAdvisorCheckResult :: Text -> DescribeTrustedAdvisorCheckResult
describeTrustedAdvisorCheckResult pCheckId = DescribeTrustedAdvisorCheckResult'{_dtacrLanguage = Nothing, _dtacrCheckId = pCheckId};

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
dtacrLanguage :: Lens' DescribeTrustedAdvisorCheckResult (Maybe Text)
dtacrLanguage = lens _dtacrLanguage (\ s a -> s{_dtacrLanguage = a});

-- | The unique identifier for the Trusted Advisor check.
dtacrCheckId :: Lens' DescribeTrustedAdvisorCheckResult Text
dtacrCheckId = lens _dtacrCheckId (\ s a -> s{_dtacrCheckId = a});

instance AWSRequest DescribeTrustedAdvisorCheckResult
         where
        type Sv DescribeTrustedAdvisorCheckResult = Support
        type Rs DescribeTrustedAdvisorCheckResult =
             DescribeTrustedAdvisorCheckResultResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTrustedAdvisorCheckResultResponse' <$>
                   (x .?> "result"))

instance ToHeaders DescribeTrustedAdvisorCheckResult
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSSupport_20130415.DescribeTrustedAdvisorCheckResult"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTrustedAdvisorCheckResult
         where
        toJSON DescribeTrustedAdvisorCheckResult'{..}
          = object
              ["language" .= _dtacrLanguage,
               "checkId" .= _dtacrCheckId]

instance ToPath DescribeTrustedAdvisorCheckResult
         where
        toPath = const "/"

instance ToQuery DescribeTrustedAdvisorCheckResult
         where
        toQuery = const mempty

-- | /See:/ 'describeTrustedAdvisorCheckResultResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtacrrResult'
newtype DescribeTrustedAdvisorCheckResultResponse = DescribeTrustedAdvisorCheckResultResponse'{_dtacrrResult :: Maybe TrustedAdvisorCheckResult} deriving (Eq, Read, Show)

-- | 'DescribeTrustedAdvisorCheckResultResponse' smart constructor.
describeTrustedAdvisorCheckResultResponse :: DescribeTrustedAdvisorCheckResultResponse
describeTrustedAdvisorCheckResultResponse = DescribeTrustedAdvisorCheckResultResponse'{_dtacrrResult = Nothing};

-- | The detailed results of the Trusted Advisor check.
dtacrrResult :: Lens' DescribeTrustedAdvisorCheckResultResponse (Maybe TrustedAdvisorCheckResult)
dtacrrResult = lens _dtacrrResult (\ s a -> s{_dtacrrResult = a});
