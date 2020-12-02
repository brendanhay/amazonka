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
-- Module      : Network.AWS.MarketplaceMetering.BatchMeterUsage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- BatchMeterUsage is called from a SaaS application listed on the AWS Marketplace to post metering records for a set of customers.
--
--
-- For identical requests, the API is idempotent; requests can be retried with the same records or a subset of the input records.
--
-- Every request to BatchMeterUsage is for one product. If you need to meter usage for multiple products, you must make multiple calls to BatchMeterUsage.
--
-- BatchMeterUsage can process up to 25 UsageRecords at a time.
--
module Network.AWS.MarketplaceMetering.BatchMeterUsage
    (
    -- * Creating a Request
      batchMeterUsage
    , BatchMeterUsage
    -- * Request Lenses
    , bmuUsageRecords
    , bmuProductCode

    -- * Destructuring the Response
    , batchMeterUsageResponse
    , BatchMeterUsageResponse
    -- * Response Lenses
    , bmursResults
    , bmursUnprocessedRecords
    , bmursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MarketplaceMetering.Types
import Network.AWS.MarketplaceMetering.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A BatchMeterUsageRequest contains UsageRecords, which indicate quantities of usage within your application.
--
--
--
-- /See:/ 'batchMeterUsage' smart constructor.
data BatchMeterUsage = BatchMeterUsage'
  { _bmuUsageRecords :: ![UsageRecord]
  , _bmuProductCode  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchMeterUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmuUsageRecords' - The set of UsageRecords to submit. BatchMeterUsage accepts up to 25 UsageRecords at a time.
--
-- * 'bmuProductCode' - Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
batchMeterUsage
    :: Text -- ^ 'bmuProductCode'
    -> BatchMeterUsage
batchMeterUsage pProductCode_ =
  BatchMeterUsage' {_bmuUsageRecords = mempty, _bmuProductCode = pProductCode_}


-- | The set of UsageRecords to submit. BatchMeterUsage accepts up to 25 UsageRecords at a time.
bmuUsageRecords :: Lens' BatchMeterUsage [UsageRecord]
bmuUsageRecords = lens _bmuUsageRecords (\ s a -> s{_bmuUsageRecords = a}) . _Coerce

-- | Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
bmuProductCode :: Lens' BatchMeterUsage Text
bmuProductCode = lens _bmuProductCode (\ s a -> s{_bmuProductCode = a})

instance AWSRequest BatchMeterUsage where
        type Rs BatchMeterUsage = BatchMeterUsageResponse
        request = postJSON marketplaceMetering
        response
          = receiveJSON
              (\ s h x ->
                 BatchMeterUsageResponse' <$>
                   (x .?> "Results" .!@ mempty) <*>
                     (x .?> "UnprocessedRecords" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable BatchMeterUsage where

instance NFData BatchMeterUsage where

instance ToHeaders BatchMeterUsage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMPMeteringService.BatchMeterUsage" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchMeterUsage where
        toJSON BatchMeterUsage'{..}
          = object
              (catMaybes
                 [Just ("UsageRecords" .= _bmuUsageRecords),
                  Just ("ProductCode" .= _bmuProductCode)])

instance ToPath BatchMeterUsage where
        toPath = const "/"

instance ToQuery BatchMeterUsage where
        toQuery = const mempty

-- | Contains the UsageRecords processed by BatchMeterUsage and any records that have failed due to transient error.
--
--
--
-- /See:/ 'batchMeterUsageResponse' smart constructor.
data BatchMeterUsageResponse = BatchMeterUsageResponse'
  { _bmursResults            :: !(Maybe [UsageRecordResult])
  , _bmursUnprocessedRecords :: !(Maybe [UsageRecord])
  , _bmursResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchMeterUsageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmursResults' - Contains all UsageRecords processed by BatchMeterUsage. These records were either honored by AWS Marketplace Metering Service or were invalid.
--
-- * 'bmursUnprocessedRecords' - Contains all UsageRecords that were not processed by BatchMeterUsage. This is a list of UsageRecords. You can retry the failed request by making another BatchMeterUsage call with this list as input in the BatchMeterUsageRequest.
--
-- * 'bmursResponseStatus' - -- | The response status code.
batchMeterUsageResponse
    :: Int -- ^ 'bmursResponseStatus'
    -> BatchMeterUsageResponse
batchMeterUsageResponse pResponseStatus_ =
  BatchMeterUsageResponse'
    { _bmursResults = Nothing
    , _bmursUnprocessedRecords = Nothing
    , _bmursResponseStatus = pResponseStatus_
    }


-- | Contains all UsageRecords processed by BatchMeterUsage. These records were either honored by AWS Marketplace Metering Service or were invalid.
bmursResults :: Lens' BatchMeterUsageResponse [UsageRecordResult]
bmursResults = lens _bmursResults (\ s a -> s{_bmursResults = a}) . _Default . _Coerce

-- | Contains all UsageRecords that were not processed by BatchMeterUsage. This is a list of UsageRecords. You can retry the failed request by making another BatchMeterUsage call with this list as input in the BatchMeterUsageRequest.
bmursUnprocessedRecords :: Lens' BatchMeterUsageResponse [UsageRecord]
bmursUnprocessedRecords = lens _bmursUnprocessedRecords (\ s a -> s{_bmursUnprocessedRecords = a}) . _Default . _Coerce

-- | -- | The response status code.
bmursResponseStatus :: Lens' BatchMeterUsageResponse Int
bmursResponseStatus = lens _bmursResponseStatus (\ s a -> s{_bmursResponseStatus = a})

instance NFData BatchMeterUsageResponse where
