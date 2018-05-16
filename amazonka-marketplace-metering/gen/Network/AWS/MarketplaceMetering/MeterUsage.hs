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
-- Module      : Network.AWS.MarketplaceMetering.MeterUsage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- API to emit metering records. For identical requests, the API is idempotent. It simply returns the metering record ID.
--
--
-- MeterUsage is authenticated on the buyer's AWS account, generally when running from an EC2 instance on the AWS Marketplace.
--
module Network.AWS.MarketplaceMetering.MeterUsage
    (
    -- * Creating a Request
      meterUsage
    , MeterUsage
    -- * Request Lenses
    , muProductCode
    , muTimestamp
    , muUsageDimension
    , muUsageQuantity
    , muDryRun

    -- * Destructuring the Response
    , meterUsageResponse
    , MeterUsageResponse
    -- * Response Lenses
    , mursMeteringRecordId
    , mursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MarketplaceMetering.Types
import Network.AWS.MarketplaceMetering.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'meterUsage' smart constructor.
data MeterUsage = MeterUsage'
  { _muProductCode    :: !Text
  , _muTimestamp      :: !POSIX
  , _muUsageDimension :: !Text
  , _muUsageQuantity  :: !Nat
  , _muDryRun         :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MeterUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'muProductCode' - Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
--
-- * 'muTimestamp' - Timestamp of the hour, recorded in UTC. The seconds and milliseconds portions of the timestamp will be ignored.
--
-- * 'muUsageDimension' - It will be one of the fcp dimension name provided during the publishing of the product.
--
-- * 'muUsageQuantity' - Consumption value for the hour.
--
-- * 'muDryRun' - Checks whether you have the permissions required for the action, but does not make the request. If you have the permissions, the request returns DryRunOperation; otherwise, it returns UnauthorizedException.
meterUsage
    :: Text -- ^ 'muProductCode'
    -> UTCTime -- ^ 'muTimestamp'
    -> Text -- ^ 'muUsageDimension'
    -> Natural -- ^ 'muUsageQuantity'
    -> Bool -- ^ 'muDryRun'
    -> MeterUsage
meterUsage pProductCode_ pTimestamp_ pUsageDimension_ pUsageQuantity_ pDryRun_ =
  MeterUsage'
    { _muProductCode = pProductCode_
    , _muTimestamp = _Time # pTimestamp_
    , _muUsageDimension = pUsageDimension_
    , _muUsageQuantity = _Nat # pUsageQuantity_
    , _muDryRun = pDryRun_
    }


-- | Product code is used to uniquely identify a product in AWS Marketplace. The product code should be the same as the one used during the publishing of a new product.
muProductCode :: Lens' MeterUsage Text
muProductCode = lens _muProductCode (\ s a -> s{_muProductCode = a})

-- | Timestamp of the hour, recorded in UTC. The seconds and milliseconds portions of the timestamp will be ignored.
muTimestamp :: Lens' MeterUsage UTCTime
muTimestamp = lens _muTimestamp (\ s a -> s{_muTimestamp = a}) . _Time

-- | It will be one of the fcp dimension name provided during the publishing of the product.
muUsageDimension :: Lens' MeterUsage Text
muUsageDimension = lens _muUsageDimension (\ s a -> s{_muUsageDimension = a})

-- | Consumption value for the hour.
muUsageQuantity :: Lens' MeterUsage Natural
muUsageQuantity = lens _muUsageQuantity (\ s a -> s{_muUsageQuantity = a}) . _Nat

-- | Checks whether you have the permissions required for the action, but does not make the request. If you have the permissions, the request returns DryRunOperation; otherwise, it returns UnauthorizedException.
muDryRun :: Lens' MeterUsage Bool
muDryRun = lens _muDryRun (\ s a -> s{_muDryRun = a})

instance AWSRequest MeterUsage where
        type Rs MeterUsage = MeterUsageResponse
        request = postJSON marketplaceMetering
        response
          = receiveJSON
              (\ s h x ->
                 MeterUsageResponse' <$>
                   (x .?> "MeteringRecordId") <*> (pure (fromEnum s)))

instance Hashable MeterUsage where

instance NFData MeterUsage where

instance ToHeaders MeterUsage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMPMeteringService.MeterUsage" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON MeterUsage where
        toJSON MeterUsage'{..}
          = object
              (catMaybes
                 [Just ("ProductCode" .= _muProductCode),
                  Just ("Timestamp" .= _muTimestamp),
                  Just ("UsageDimension" .= _muUsageDimension),
                  Just ("UsageQuantity" .= _muUsageQuantity),
                  Just ("DryRun" .= _muDryRun)])

instance ToPath MeterUsage where
        toPath = const "/"

instance ToQuery MeterUsage where
        toQuery = const mempty

-- | /See:/ 'meterUsageResponse' smart constructor.
data MeterUsageResponse = MeterUsageResponse'
  { _mursMeteringRecordId :: !(Maybe Text)
  , _mursResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'MeterUsageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mursMeteringRecordId' - Undocumented member.
--
-- * 'mursResponseStatus' - -- | The response status code.
meterUsageResponse
    :: Int -- ^ 'mursResponseStatus'
    -> MeterUsageResponse
meterUsageResponse pResponseStatus_ =
  MeterUsageResponse'
    {_mursMeteringRecordId = Nothing, _mursResponseStatus = pResponseStatus_}


-- | Undocumented member.
mursMeteringRecordId :: Lens' MeterUsageResponse (Maybe Text)
mursMeteringRecordId = lens _mursMeteringRecordId (\ s a -> s{_mursMeteringRecordId = a})

-- | -- | The response status code.
mursResponseStatus :: Lens' MeterUsageResponse Int
mursResponseStatus = lens _mursResponseStatus (\ s a -> s{_mursResponseStatus = a})

instance NFData MeterUsageResponse where
