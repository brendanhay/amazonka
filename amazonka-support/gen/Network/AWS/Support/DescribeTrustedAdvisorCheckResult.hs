{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckResult
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the results of the Trusted Advisor check that has the specified
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
    , dtacrrqLanguage
    , dtacrrqCheckId

    -- * Response
    , DescribeTrustedAdvisorCheckResultResponse
    -- ** Response constructor
    , describeTrustedAdvisorCheckResultResponse
    -- ** Response lenses
    , dtacrrsResult
    , dtacrrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Support.Types

-- | /See:/ 'describeTrustedAdvisorCheckResult' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtacrrqLanguage'
--
-- * 'dtacrrqCheckId'
data DescribeTrustedAdvisorCheckResult = DescribeTrustedAdvisorCheckResult'
    { _dtacrrqLanguage :: !(Maybe Text)
    , _dtacrrqCheckId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTrustedAdvisorCheckResult' smart constructor.
describeTrustedAdvisorCheckResult :: Text -> DescribeTrustedAdvisorCheckResult
describeTrustedAdvisorCheckResult pCheckId_ =
    DescribeTrustedAdvisorCheckResult'
    { _dtacrrqLanguage = Nothing
    , _dtacrrqCheckId = pCheckId_
    }

-- | The ISO 639-1 code for the language in which AWS provides support. AWS
-- Support currently supports English (\"en\") and Japanese (\"ja\").
-- Language parameters must be passed explicitly for operations that take
-- them.
dtacrrqLanguage :: Lens' DescribeTrustedAdvisorCheckResult (Maybe Text)
dtacrrqLanguage = lens _dtacrrqLanguage (\ s a -> s{_dtacrrqLanguage = a});

-- | The unique identifier for the Trusted Advisor check.
dtacrrqCheckId :: Lens' DescribeTrustedAdvisorCheckResult Text
dtacrrqCheckId = lens _dtacrrqCheckId (\ s a -> s{_dtacrrqCheckId = a});

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
                   (x .?> "result") <*> (pure (fromEnum s)))

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
              ["language" .= _dtacrrqLanguage,
               "checkId" .= _dtacrrqCheckId]

instance ToPath DescribeTrustedAdvisorCheckResult
         where
        toPath = const "/"

instance ToQuery DescribeTrustedAdvisorCheckResult
         where
        toQuery = const mempty

-- | The result of the Trusted Advisor check returned by the
-- DescribeTrustedAdvisorCheckResult operation.
--
-- /See:/ 'describeTrustedAdvisorCheckResultResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtacrrsResult'
--
-- * 'dtacrrsStatus'
data DescribeTrustedAdvisorCheckResultResponse = DescribeTrustedAdvisorCheckResultResponse'
    { _dtacrrsResult :: !(Maybe TrustedAdvisorCheckResult)
    , _dtacrrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeTrustedAdvisorCheckResultResponse' smart constructor.
describeTrustedAdvisorCheckResultResponse :: Int -> DescribeTrustedAdvisorCheckResultResponse
describeTrustedAdvisorCheckResultResponse pStatus_ =
    DescribeTrustedAdvisorCheckResultResponse'
    { _dtacrrsResult = Nothing
    , _dtacrrsStatus = pStatus_
    }

-- | The detailed results of the Trusted Advisor check.
dtacrrsResult :: Lens' DescribeTrustedAdvisorCheckResultResponse (Maybe TrustedAdvisorCheckResult)
dtacrrsResult = lens _dtacrrsResult (\ s a -> s{_dtacrrsResult = a});

-- | FIXME: Undocumented member.
dtacrrsStatus :: Lens' DescribeTrustedAdvisorCheckResultResponse Int
dtacrrsStatus = lens _dtacrrsStatus (\ s a -> s{_dtacrrsStatus = a});
