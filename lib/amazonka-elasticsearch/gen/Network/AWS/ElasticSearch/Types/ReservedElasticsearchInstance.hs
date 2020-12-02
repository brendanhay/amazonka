{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstance where

import Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
import Network.AWS.ElasticSearch.Types.RecurringCharge
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details of a reserved Elasticsearch instance.
--
--
--
-- /See:/ 'reservedElasticsearchInstance' smart constructor.
data ReservedElasticsearchInstance = ReservedElasticsearchInstance'
  { _reiState ::
      !(Maybe Text),
    _reiCurrencyCode ::
      !(Maybe Text),
    _reiStartTime :: !(Maybe POSIX),
    _reiReservedElasticsearchInstanceOfferingId ::
      !(Maybe Text),
    _reiReservedElasticsearchInstanceId ::
      !(Maybe Text),
    _reiElasticsearchInstanceCount ::
      !(Maybe Int),
    _reiReservationName ::
      !(Maybe Text),
    _reiElasticsearchInstanceType ::
      !( Maybe
           ESPartitionInstanceType
       ),
    _reiRecurringCharges ::
      !(Maybe [RecurringCharge]),
    _reiUsagePrice ::
      !(Maybe Double),
    _reiFixedPrice ::
      !(Maybe Double),
    _reiDuration :: !(Maybe Int),
    _reiPaymentOption ::
      !( Maybe
           ReservedElasticsearchInstancePaymentOption
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservedElasticsearchInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reiState' - The state of the reserved Elasticsearch instance.
--
-- * 'reiCurrencyCode' - The currency code for the reserved Elasticsearch instance offering.
--
-- * 'reiStartTime' - The time the reservation started.
--
-- * 'reiReservedElasticsearchInstanceOfferingId' - The offering identifier.
--
-- * 'reiReservedElasticsearchInstanceId' - The unique identifier for the reservation.
--
-- * 'reiElasticsearchInstanceCount' - The number of Elasticsearch instances that have been reserved.
--
-- * 'reiReservationName' - The customer-specified identifier to track this reservation.
--
-- * 'reiElasticsearchInstanceType' - The Elasticsearch instance type offered by the reserved instance offering.
--
-- * 'reiRecurringCharges' - The charge to your account regardless of whether you are creating any domains using the instance offering.
--
-- * 'reiUsagePrice' - The rate you are charged for each hour for the domain that is using this reserved instance.
--
-- * 'reiFixedPrice' - The upfront fixed charge you will paid to purchase the specific reserved Elasticsearch instance offering.
--
-- * 'reiDuration' - The duration, in seconds, for which the Elasticsearch instance is reserved.
--
-- * 'reiPaymentOption' - The payment option as defined in the reserved Elasticsearch instance offering.
reservedElasticsearchInstance ::
  ReservedElasticsearchInstance
reservedElasticsearchInstance =
  ReservedElasticsearchInstance'
    { _reiState = Nothing,
      _reiCurrencyCode = Nothing,
      _reiStartTime = Nothing,
      _reiReservedElasticsearchInstanceOfferingId = Nothing,
      _reiReservedElasticsearchInstanceId = Nothing,
      _reiElasticsearchInstanceCount = Nothing,
      _reiReservationName = Nothing,
      _reiElasticsearchInstanceType = Nothing,
      _reiRecurringCharges = Nothing,
      _reiUsagePrice = Nothing,
      _reiFixedPrice = Nothing,
      _reiDuration = Nothing,
      _reiPaymentOption = Nothing
    }

-- | The state of the reserved Elasticsearch instance.
reiState :: Lens' ReservedElasticsearchInstance (Maybe Text)
reiState = lens _reiState (\s a -> s {_reiState = a})

-- | The currency code for the reserved Elasticsearch instance offering.
reiCurrencyCode :: Lens' ReservedElasticsearchInstance (Maybe Text)
reiCurrencyCode = lens _reiCurrencyCode (\s a -> s {_reiCurrencyCode = a})

-- | The time the reservation started.
reiStartTime :: Lens' ReservedElasticsearchInstance (Maybe UTCTime)
reiStartTime = lens _reiStartTime (\s a -> s {_reiStartTime = a}) . mapping _Time

-- | The offering identifier.
reiReservedElasticsearchInstanceOfferingId :: Lens' ReservedElasticsearchInstance (Maybe Text)
reiReservedElasticsearchInstanceOfferingId = lens _reiReservedElasticsearchInstanceOfferingId (\s a -> s {_reiReservedElasticsearchInstanceOfferingId = a})

-- | The unique identifier for the reservation.
reiReservedElasticsearchInstanceId :: Lens' ReservedElasticsearchInstance (Maybe Text)
reiReservedElasticsearchInstanceId = lens _reiReservedElasticsearchInstanceId (\s a -> s {_reiReservedElasticsearchInstanceId = a})

-- | The number of Elasticsearch instances that have been reserved.
reiElasticsearchInstanceCount :: Lens' ReservedElasticsearchInstance (Maybe Int)
reiElasticsearchInstanceCount = lens _reiElasticsearchInstanceCount (\s a -> s {_reiElasticsearchInstanceCount = a})

-- | The customer-specified identifier to track this reservation.
reiReservationName :: Lens' ReservedElasticsearchInstance (Maybe Text)
reiReservationName = lens _reiReservationName (\s a -> s {_reiReservationName = a})

-- | The Elasticsearch instance type offered by the reserved instance offering.
reiElasticsearchInstanceType :: Lens' ReservedElasticsearchInstance (Maybe ESPartitionInstanceType)
reiElasticsearchInstanceType = lens _reiElasticsearchInstanceType (\s a -> s {_reiElasticsearchInstanceType = a})

-- | The charge to your account regardless of whether you are creating any domains using the instance offering.
reiRecurringCharges :: Lens' ReservedElasticsearchInstance [RecurringCharge]
reiRecurringCharges = lens _reiRecurringCharges (\s a -> s {_reiRecurringCharges = a}) . _Default . _Coerce

-- | The rate you are charged for each hour for the domain that is using this reserved instance.
reiUsagePrice :: Lens' ReservedElasticsearchInstance (Maybe Double)
reiUsagePrice = lens _reiUsagePrice (\s a -> s {_reiUsagePrice = a})

-- | The upfront fixed charge you will paid to purchase the specific reserved Elasticsearch instance offering.
reiFixedPrice :: Lens' ReservedElasticsearchInstance (Maybe Double)
reiFixedPrice = lens _reiFixedPrice (\s a -> s {_reiFixedPrice = a})

-- | The duration, in seconds, for which the Elasticsearch instance is reserved.
reiDuration :: Lens' ReservedElasticsearchInstance (Maybe Int)
reiDuration = lens _reiDuration (\s a -> s {_reiDuration = a})

-- | The payment option as defined in the reserved Elasticsearch instance offering.
reiPaymentOption :: Lens' ReservedElasticsearchInstance (Maybe ReservedElasticsearchInstancePaymentOption)
reiPaymentOption = lens _reiPaymentOption (\s a -> s {_reiPaymentOption = a})

instance FromJSON ReservedElasticsearchInstance where
  parseJSON =
    withObject
      "ReservedElasticsearchInstance"
      ( \x ->
          ReservedElasticsearchInstance'
            <$> (x .:? "State")
            <*> (x .:? "CurrencyCode")
            <*> (x .:? "StartTime")
            <*> (x .:? "ReservedElasticsearchInstanceOfferingId")
            <*> (x .:? "ReservedElasticsearchInstanceId")
            <*> (x .:? "ElasticsearchInstanceCount")
            <*> (x .:? "ReservationName")
            <*> (x .:? "ElasticsearchInstanceType")
            <*> (x .:? "RecurringCharges" .!= mempty)
            <*> (x .:? "UsagePrice")
            <*> (x .:? "FixedPrice")
            <*> (x .:? "Duration")
            <*> (x .:? "PaymentOption")
      )

instance Hashable ReservedElasticsearchInstance

instance NFData ReservedElasticsearchInstance
