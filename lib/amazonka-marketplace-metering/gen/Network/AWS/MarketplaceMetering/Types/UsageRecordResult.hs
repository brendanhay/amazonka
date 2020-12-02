{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.Types.UsageRecordResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceMetering.Types.UsageRecordResult where

import Network.AWS.Lens
import Network.AWS.MarketplaceMetering.Types.UsageRecord
import Network.AWS.MarketplaceMetering.Types.UsageRecordResultStatus
import Network.AWS.Prelude

-- | A UsageRecordResult indicates the status of a given UsageRecord processed by BatchMeterUsage.
--
--
--
-- /See:/ 'usageRecordResult' smart constructor.
data UsageRecordResult = UsageRecordResult'
  { _urrStatus ::
      !(Maybe UsageRecordResultStatus),
    _urrUsageRecord :: !(Maybe UsageRecord),
    _urrMeteringRecordId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UsageRecordResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrStatus' - The UsageRecordResult Status indicates the status of an individual UsageRecord processed by BatchMeterUsage.     * /Success/ - The UsageRecord was accepted and honored by BatchMeterUsage.     * /CustomerNotSubscribed/ - The CustomerIdentifier specified is not subscribed to your product. The UsageRecord was not honored. Future UsageRecords for this customer will fail until the customer subscribes to your product.     * /DuplicateRecord/ - Indicates that the UsageRecord was invalid and not honored. A previously metered UsageRecord had the same customer, dimension, and time, but a different quantity.
--
-- * 'urrUsageRecord' - The UsageRecord that was part of the BatchMeterUsage request.
--
-- * 'urrMeteringRecordId' - The MeteringRecordId is a unique identifier for this metering event.
usageRecordResult ::
  UsageRecordResult
usageRecordResult =
  UsageRecordResult'
    { _urrStatus = Nothing,
      _urrUsageRecord = Nothing,
      _urrMeteringRecordId = Nothing
    }

-- | The UsageRecordResult Status indicates the status of an individual UsageRecord processed by BatchMeterUsage.     * /Success/ - The UsageRecord was accepted and honored by BatchMeterUsage.     * /CustomerNotSubscribed/ - The CustomerIdentifier specified is not subscribed to your product. The UsageRecord was not honored. Future UsageRecords for this customer will fail until the customer subscribes to your product.     * /DuplicateRecord/ - Indicates that the UsageRecord was invalid and not honored. A previously metered UsageRecord had the same customer, dimension, and time, but a different quantity.
urrStatus :: Lens' UsageRecordResult (Maybe UsageRecordResultStatus)
urrStatus = lens _urrStatus (\s a -> s {_urrStatus = a})

-- | The UsageRecord that was part of the BatchMeterUsage request.
urrUsageRecord :: Lens' UsageRecordResult (Maybe UsageRecord)
urrUsageRecord = lens _urrUsageRecord (\s a -> s {_urrUsageRecord = a})

-- | The MeteringRecordId is a unique identifier for this metering event.
urrMeteringRecordId :: Lens' UsageRecordResult (Maybe Text)
urrMeteringRecordId = lens _urrMeteringRecordId (\s a -> s {_urrMeteringRecordId = a})

instance FromJSON UsageRecordResult where
  parseJSON =
    withObject
      "UsageRecordResult"
      ( \x ->
          UsageRecordResult'
            <$> (x .:? "Status")
            <*> (x .:? "UsageRecord")
            <*> (x .:? "MeteringRecordId")
      )

instance Hashable UsageRecordResult

instance NFData UsageRecordResult
