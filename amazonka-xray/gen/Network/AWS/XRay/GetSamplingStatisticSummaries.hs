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
-- Module      : Network.AWS.XRay.GetSamplingStatisticSummaries
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about recent sampling results for all sampling rules.
--
--
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetSamplingStatisticSummaries
    (
    -- * Creating a Request
      getSamplingStatisticSummaries
    , GetSamplingStatisticSummaries
    -- * Request Lenses
    , gsssNextToken

    -- * Destructuring the Response
    , getSamplingStatisticSummariesResponse
    , GetSamplingStatisticSummariesResponse
    -- * Response Lenses
    , gsssrsSamplingStatisticSummaries
    , gsssrsNextToken
    , gsssrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types
import Network.AWS.XRay.Types.Product

-- | /See:/ 'getSamplingStatisticSummaries' smart constructor.
newtype GetSamplingStatisticSummaries = GetSamplingStatisticSummaries'
  { _gsssNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSamplingStatisticSummaries' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsssNextToken' - Pagination token. Not used.
getSamplingStatisticSummaries
    :: GetSamplingStatisticSummaries
getSamplingStatisticSummaries =
  GetSamplingStatisticSummaries' {_gsssNextToken = Nothing}


-- | Pagination token. Not used.
gsssNextToken :: Lens' GetSamplingStatisticSummaries (Maybe Text)
gsssNextToken = lens _gsssNextToken (\ s a -> s{_gsssNextToken = a})

instance AWSPager GetSamplingStatisticSummaries where
        page rq rs
          | stop (rs ^. gsssrsNextToken) = Nothing
          | stop (rs ^. gsssrsSamplingStatisticSummaries) =
            Nothing
          | otherwise =
            Just $ rq & gsssNextToken .~ rs ^. gsssrsNextToken

instance AWSRequest GetSamplingStatisticSummaries
         where
        type Rs GetSamplingStatisticSummaries =
             GetSamplingStatisticSummariesResponse
        request = postJSON xRay
        response
          = receiveJSON
              (\ s h x ->
                 GetSamplingStatisticSummariesResponse' <$>
                   (x .?> "SamplingStatisticSummaries" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetSamplingStatisticSummaries where

instance NFData GetSamplingStatisticSummaries where

instance ToHeaders GetSamplingStatisticSummaries
         where
        toHeaders = const mempty

instance ToJSON GetSamplingStatisticSummaries where
        toJSON GetSamplingStatisticSummaries'{..}
          = object
              (catMaybes [("NextToken" .=) <$> _gsssNextToken])

instance ToPath GetSamplingStatisticSummaries where
        toPath = const "/SamplingStatisticSummaries"

instance ToQuery GetSamplingStatisticSummaries where
        toQuery = const mempty

-- | /See:/ 'getSamplingStatisticSummariesResponse' smart constructor.
data GetSamplingStatisticSummariesResponse = GetSamplingStatisticSummariesResponse'
  { _gsssrsSamplingStatisticSummaries :: !(Maybe [SamplingStatisticSummary])
  , _gsssrsNextToken :: !(Maybe Text)
  , _gsssrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSamplingStatisticSummariesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsssrsSamplingStatisticSummaries' - Information about the number of requests instrumented for each sampling rule.
--
-- * 'gsssrsNextToken' - Pagination token. Not used.
--
-- * 'gsssrsResponseStatus' - -- | The response status code.
getSamplingStatisticSummariesResponse
    :: Int -- ^ 'gsssrsResponseStatus'
    -> GetSamplingStatisticSummariesResponse
getSamplingStatisticSummariesResponse pResponseStatus_ =
  GetSamplingStatisticSummariesResponse'
    { _gsssrsSamplingStatisticSummaries = Nothing
    , _gsssrsNextToken = Nothing
    , _gsssrsResponseStatus = pResponseStatus_
    }


-- | Information about the number of requests instrumented for each sampling rule.
gsssrsSamplingStatisticSummaries :: Lens' GetSamplingStatisticSummariesResponse [SamplingStatisticSummary]
gsssrsSamplingStatisticSummaries = lens _gsssrsSamplingStatisticSummaries (\ s a -> s{_gsssrsSamplingStatisticSummaries = a}) . _Default . _Coerce

-- | Pagination token. Not used.
gsssrsNextToken :: Lens' GetSamplingStatisticSummariesResponse (Maybe Text)
gsssrsNextToken = lens _gsssrsNextToken (\ s a -> s{_gsssrsNextToken = a})

-- | -- | The response status code.
gsssrsResponseStatus :: Lens' GetSamplingStatisticSummariesResponse Int
gsssrsResponseStatus = lens _gsssrsResponseStatus (\ s a -> s{_gsssrsResponseStatus = a})

instance NFData GetSamplingStatisticSummariesResponse
         where
