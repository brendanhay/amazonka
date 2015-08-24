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
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckResult
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/awssupport/latest/APIReference/API_DescribeTrustedAdvisorCheckResult.html AWS API Reference> for DescribeTrustedAdvisorCheckResult.
module Network.AWS.Support.DescribeTrustedAdvisorCheckResult
    (
    -- * Creating a Request
      describeTrustedAdvisorCheckResult
    , DescribeTrustedAdvisorCheckResult
    -- * Request Lenses
    , dtacrLanguage
    , dtacrCheckId

    -- * Destructuring the Response
    , describeTrustedAdvisorCheckResultResponse
    , DescribeTrustedAdvisorCheckResultResponse
    -- * Response Lenses
    , dtacrrsResult
    , dtacrrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Support.Types
import           Network.AWS.Support.Types.Product

-- | /See:/ 'describeTrustedAdvisorCheckResult' smart constructor.
data DescribeTrustedAdvisorCheckResult = DescribeTrustedAdvisorCheckResult'
    { _dtacrLanguage :: !(Maybe Text)
    , _dtacrCheckId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeTrustedAdvisorCheckResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtacrLanguage'
--
-- * 'dtacrCheckId'
describeTrustedAdvisorCheckResult
    :: Text -- ^ 'dtacrCheckId'
    -> DescribeTrustedAdvisorCheckResult
describeTrustedAdvisorCheckResult pCheckId_ =
    DescribeTrustedAdvisorCheckResult'
    { _dtacrLanguage = Nothing
    , _dtacrCheckId = pCheckId_
    }

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
        type Rs DescribeTrustedAdvisorCheckResult =
             DescribeTrustedAdvisorCheckResultResponse
        request = postJSON support
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
              (catMaybes
                 [("language" .=) <$> _dtacrLanguage,
                  Just ("checkId" .= _dtacrCheckId)])

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
data DescribeTrustedAdvisorCheckResultResponse = DescribeTrustedAdvisorCheckResultResponse'
    { _dtacrrsResult :: !(Maybe TrustedAdvisorCheckResult)
    , _dtacrrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeTrustedAdvisorCheckResultResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtacrrsResult'
--
-- * 'dtacrrsStatus'
describeTrustedAdvisorCheckResultResponse
    :: Int -- ^ 'dtacrrsStatus'
    -> DescribeTrustedAdvisorCheckResultResponse
describeTrustedAdvisorCheckResultResponse pStatus_ =
    DescribeTrustedAdvisorCheckResultResponse'
    { _dtacrrsResult = Nothing
    , _dtacrrsStatus = pStatus_
    }

-- | The detailed results of the Trusted Advisor check.
dtacrrsResult :: Lens' DescribeTrustedAdvisorCheckResultResponse (Maybe TrustedAdvisorCheckResult)
dtacrrsResult = lens _dtacrrsResult (\ s a -> s{_dtacrrsResult = a});

-- | The response status code.
dtacrrsStatus :: Lens' DescribeTrustedAdvisorCheckResultResponse Int
dtacrrsStatus = lens _dtacrrsStatus (\ s a -> s{_dtacrrsStatus = a});
