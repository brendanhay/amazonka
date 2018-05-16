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
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckSummaries
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the summaries of the results of the Trusted Advisor checks that have the specified check IDs. Check IDs can be obtained by calling 'DescribeTrustedAdvisorChecks' .
--
--
-- The response contains an array of 'TrustedAdvisorCheckSummary' objects.
--
module Network.AWS.Support.DescribeTrustedAdvisorCheckSummaries
    (
    -- * Creating a Request
      describeTrustedAdvisorCheckSummaries
    , DescribeTrustedAdvisorCheckSummaries
    -- * Request Lenses
    , dtacsCheckIds

    -- * Destructuring the Response
    , describeTrustedAdvisorCheckSummariesResponse
    , DescribeTrustedAdvisorCheckSummariesResponse
    -- * Response Lenses
    , dtacsrsResponseStatus
    , dtacsrsSummaries
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Support.Types
import Network.AWS.Support.Types.Product

-- |
--
--
--
-- /See:/ 'describeTrustedAdvisorCheckSummaries' smart constructor.
newtype DescribeTrustedAdvisorCheckSummaries = DescribeTrustedAdvisorCheckSummaries'
  { _dtacsCheckIds :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTrustedAdvisorCheckSummaries' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtacsCheckIds' - The IDs of the Trusted Advisor checks.
describeTrustedAdvisorCheckSummaries
    :: DescribeTrustedAdvisorCheckSummaries
describeTrustedAdvisorCheckSummaries =
  DescribeTrustedAdvisorCheckSummaries' {_dtacsCheckIds = mempty}


-- | The IDs of the Trusted Advisor checks.
dtacsCheckIds :: Lens' DescribeTrustedAdvisorCheckSummaries [Text]
dtacsCheckIds = lens _dtacsCheckIds (\ s a -> s{_dtacsCheckIds = a}) . _Coerce

instance AWSRequest
           DescribeTrustedAdvisorCheckSummaries
         where
        type Rs DescribeTrustedAdvisorCheckSummaries =
             DescribeTrustedAdvisorCheckSummariesResponse
        request = postJSON support
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTrustedAdvisorCheckSummariesResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "summaries" .!@ mempty))

instance Hashable
           DescribeTrustedAdvisorCheckSummaries
         where

instance NFData DescribeTrustedAdvisorCheckSummaries
         where

instance ToHeaders
           DescribeTrustedAdvisorCheckSummaries
         where
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
          = object
              (catMaybes [Just ("checkIds" .= _dtacsCheckIds)])

instance ToPath DescribeTrustedAdvisorCheckSummaries
         where
        toPath = const "/"

instance ToQuery DescribeTrustedAdvisorCheckSummaries
         where
        toQuery = const mempty

-- | The summaries of the Trusted Advisor checks returned by the 'DescribeTrustedAdvisorCheckSummaries' operation.
--
--
--
-- /See:/ 'describeTrustedAdvisorCheckSummariesResponse' smart constructor.
data DescribeTrustedAdvisorCheckSummariesResponse = DescribeTrustedAdvisorCheckSummariesResponse'
  { _dtacsrsResponseStatus :: !Int
  , _dtacsrsSummaries      :: ![TrustedAdvisorCheckSummary]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTrustedAdvisorCheckSummariesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtacsrsResponseStatus' - -- | The response status code.
--
-- * 'dtacsrsSummaries' - The summary information for the requested Trusted Advisor checks.
describeTrustedAdvisorCheckSummariesResponse
    :: Int -- ^ 'dtacsrsResponseStatus'
    -> DescribeTrustedAdvisorCheckSummariesResponse
describeTrustedAdvisorCheckSummariesResponse pResponseStatus_ =
  DescribeTrustedAdvisorCheckSummariesResponse'
    {_dtacsrsResponseStatus = pResponseStatus_, _dtacsrsSummaries = mempty}


-- | -- | The response status code.
dtacsrsResponseStatus :: Lens' DescribeTrustedAdvisorCheckSummariesResponse Int
dtacsrsResponseStatus = lens _dtacsrsResponseStatus (\ s a -> s{_dtacsrsResponseStatus = a})

-- | The summary information for the requested Trusted Advisor checks.
dtacsrsSummaries :: Lens' DescribeTrustedAdvisorCheckSummariesResponse [TrustedAdvisorCheckSummary]
dtacsrsSummaries = lens _dtacsrsSummaries (\ s a -> s{_dtacsrsSummaries = a}) . _Coerce

instance NFData
           DescribeTrustedAdvisorCheckSummariesResponse
         where
