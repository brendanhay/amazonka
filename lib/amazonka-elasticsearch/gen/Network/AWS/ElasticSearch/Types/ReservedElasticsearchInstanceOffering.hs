{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceOffering where

import Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
import Network.AWS.ElasticSearch.Types.RecurringCharge
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details of a reserved Elasticsearch instance offering.
--
--
--
-- /See:/ 'reservedElasticsearchInstanceOffering' smart constructor.
data ReservedElasticsearchInstanceOffering = ReservedElasticsearchInstanceOffering'
  { _reioCurrencyCode ::
      !(Maybe Text),
    _reioReservedElasticsearchInstanceOfferingId ::
      !(Maybe Text),
    _reioElasticsearchInstanceType ::
      !( Maybe
           ESPartitionInstanceType
       ),
    _reioRecurringCharges ::
      !( Maybe
           [RecurringCharge]
       ),
    _reioUsagePrice ::
      !(Maybe Double),
    _reioFixedPrice ::
      !(Maybe Double),
    _reioDuration ::
      !(Maybe Int),
    _reioPaymentOption ::
      !( Maybe
           ReservedElasticsearchInstancePaymentOption
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservedElasticsearchInstanceOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reioCurrencyCode' - The currency code for the reserved Elasticsearch instance offering.
--
-- * 'reioReservedElasticsearchInstanceOfferingId' - The Elasticsearch reserved instance offering identifier.
--
-- * 'reioElasticsearchInstanceType' - The Elasticsearch instance type offered by the reserved instance offering.
--
-- * 'reioRecurringCharges' - The charge to your account regardless of whether you are creating any domains using the instance offering.
--
-- * 'reioUsagePrice' - The rate you are charged for each hour the domain that is using the offering is running.
--
-- * 'reioFixedPrice' - The upfront fixed charge you will pay to purchase the specific reserved Elasticsearch instance offering.
--
-- * 'reioDuration' - The duration, in seconds, for which the offering will reserve the Elasticsearch instance.
--
-- * 'reioPaymentOption' - Payment option for the reserved Elasticsearch instance offering
reservedElasticsearchInstanceOffering ::
  ReservedElasticsearchInstanceOffering
reservedElasticsearchInstanceOffering =
  ReservedElasticsearchInstanceOffering'
    { _reioCurrencyCode =
        Nothing,
      _reioReservedElasticsearchInstanceOfferingId = Nothing,
      _reioElasticsearchInstanceType = Nothing,
      _reioRecurringCharges = Nothing,
      _reioUsagePrice = Nothing,
      _reioFixedPrice = Nothing,
      _reioDuration = Nothing,
      _reioPaymentOption = Nothing
    }

-- | The currency code for the reserved Elasticsearch instance offering.
reioCurrencyCode :: Lens' ReservedElasticsearchInstanceOffering (Maybe Text)
reioCurrencyCode = lens _reioCurrencyCode (\s a -> s {_reioCurrencyCode = a})

-- | The Elasticsearch reserved instance offering identifier.
reioReservedElasticsearchInstanceOfferingId :: Lens' ReservedElasticsearchInstanceOffering (Maybe Text)
reioReservedElasticsearchInstanceOfferingId = lens _reioReservedElasticsearchInstanceOfferingId (\s a -> s {_reioReservedElasticsearchInstanceOfferingId = a})

-- | The Elasticsearch instance type offered by the reserved instance offering.
reioElasticsearchInstanceType :: Lens' ReservedElasticsearchInstanceOffering (Maybe ESPartitionInstanceType)
reioElasticsearchInstanceType = lens _reioElasticsearchInstanceType (\s a -> s {_reioElasticsearchInstanceType = a})

-- | The charge to your account regardless of whether you are creating any domains using the instance offering.
reioRecurringCharges :: Lens' ReservedElasticsearchInstanceOffering [RecurringCharge]
reioRecurringCharges = lens _reioRecurringCharges (\s a -> s {_reioRecurringCharges = a}) . _Default . _Coerce

-- | The rate you are charged for each hour the domain that is using the offering is running.
reioUsagePrice :: Lens' ReservedElasticsearchInstanceOffering (Maybe Double)
reioUsagePrice = lens _reioUsagePrice (\s a -> s {_reioUsagePrice = a})

-- | The upfront fixed charge you will pay to purchase the specific reserved Elasticsearch instance offering.
reioFixedPrice :: Lens' ReservedElasticsearchInstanceOffering (Maybe Double)
reioFixedPrice = lens _reioFixedPrice (\s a -> s {_reioFixedPrice = a})

-- | The duration, in seconds, for which the offering will reserve the Elasticsearch instance.
reioDuration :: Lens' ReservedElasticsearchInstanceOffering (Maybe Int)
reioDuration = lens _reioDuration (\s a -> s {_reioDuration = a})

-- | Payment option for the reserved Elasticsearch instance offering
reioPaymentOption :: Lens' ReservedElasticsearchInstanceOffering (Maybe ReservedElasticsearchInstancePaymentOption)
reioPaymentOption = lens _reioPaymentOption (\s a -> s {_reioPaymentOption = a})

instance FromJSON ReservedElasticsearchInstanceOffering where
  parseJSON =
    withObject
      "ReservedElasticsearchInstanceOffering"
      ( \x ->
          ReservedElasticsearchInstanceOffering'
            <$> (x .:? "CurrencyCode")
            <*> (x .:? "ReservedElasticsearchInstanceOfferingId")
            <*> (x .:? "ElasticsearchInstanceType")
            <*> (x .:? "RecurringCharges" .!= mempty)
            <*> (x .:? "UsagePrice")
            <*> (x .:? "FixedPrice")
            <*> (x .:? "Duration")
            <*> (x .:? "PaymentOption")
      )

instance Hashable ReservedElasticsearchInstanceOffering

instance NFData ReservedElasticsearchInstanceOffering
