{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ReservedNodeOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ReservedNodeOffering where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.RecurringCharge
import Network.AWS.Redshift.Types.ReservedNodeOfferingType

-- | Describes a reserved node offering.
--
--
--
-- /See:/ 'reservedNodeOffering' smart constructor.
data ReservedNodeOffering = ReservedNodeOffering'
  { _rnoReservedNodeOfferingType ::
      !(Maybe ReservedNodeOfferingType),
    _rnoCurrencyCode :: !(Maybe Text),
    _rnoReservedNodeOfferingId :: !(Maybe Text),
    _rnoRecurringCharges ::
      !(Maybe [RecurringCharge]),
    _rnoOfferingType :: !(Maybe Text),
    _rnoUsagePrice :: !(Maybe Double),
    _rnoNodeType :: !(Maybe Text),
    _rnoFixedPrice :: !(Maybe Double),
    _rnoDuration :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservedNodeOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rnoReservedNodeOfferingType' -
--
-- * 'rnoCurrencyCode' - The currency code for the compute nodes offering.
--
-- * 'rnoReservedNodeOfferingId' - The offering identifier.
--
-- * 'rnoRecurringCharges' - The charge to your account regardless of whether you are creating any clusters using the node offering. Recurring charges are only in effect for heavy-utilization reserved nodes.
--
-- * 'rnoOfferingType' - The anticipated utilization of the reserved node, as defined in the reserved node offering.
--
-- * 'rnoUsagePrice' - The rate you are charged for each hour the cluster that is using the offering is running.
--
-- * 'rnoNodeType' - The node type offered by the reserved node offering.
--
-- * 'rnoFixedPrice' - The upfront fixed charge you will pay to purchase the specific reserved node offering.
--
-- * 'rnoDuration' - The duration, in seconds, for which the offering will reserve the node.
reservedNodeOffering ::
  ReservedNodeOffering
reservedNodeOffering =
  ReservedNodeOffering'
    { _rnoReservedNodeOfferingType = Nothing,
      _rnoCurrencyCode = Nothing,
      _rnoReservedNodeOfferingId = Nothing,
      _rnoRecurringCharges = Nothing,
      _rnoOfferingType = Nothing,
      _rnoUsagePrice = Nothing,
      _rnoNodeType = Nothing,
      _rnoFixedPrice = Nothing,
      _rnoDuration = Nothing
    }

-- |
rnoReservedNodeOfferingType :: Lens' ReservedNodeOffering (Maybe ReservedNodeOfferingType)
rnoReservedNodeOfferingType = lens _rnoReservedNodeOfferingType (\s a -> s {_rnoReservedNodeOfferingType = a})

-- | The currency code for the compute nodes offering.
rnoCurrencyCode :: Lens' ReservedNodeOffering (Maybe Text)
rnoCurrencyCode = lens _rnoCurrencyCode (\s a -> s {_rnoCurrencyCode = a})

-- | The offering identifier.
rnoReservedNodeOfferingId :: Lens' ReservedNodeOffering (Maybe Text)
rnoReservedNodeOfferingId = lens _rnoReservedNodeOfferingId (\s a -> s {_rnoReservedNodeOfferingId = a})

-- | The charge to your account regardless of whether you are creating any clusters using the node offering. Recurring charges are only in effect for heavy-utilization reserved nodes.
rnoRecurringCharges :: Lens' ReservedNodeOffering [RecurringCharge]
rnoRecurringCharges = lens _rnoRecurringCharges (\s a -> s {_rnoRecurringCharges = a}) . _Default . _Coerce

-- | The anticipated utilization of the reserved node, as defined in the reserved node offering.
rnoOfferingType :: Lens' ReservedNodeOffering (Maybe Text)
rnoOfferingType = lens _rnoOfferingType (\s a -> s {_rnoOfferingType = a})

-- | The rate you are charged for each hour the cluster that is using the offering is running.
rnoUsagePrice :: Lens' ReservedNodeOffering (Maybe Double)
rnoUsagePrice = lens _rnoUsagePrice (\s a -> s {_rnoUsagePrice = a})

-- | The node type offered by the reserved node offering.
rnoNodeType :: Lens' ReservedNodeOffering (Maybe Text)
rnoNodeType = lens _rnoNodeType (\s a -> s {_rnoNodeType = a})

-- | The upfront fixed charge you will pay to purchase the specific reserved node offering.
rnoFixedPrice :: Lens' ReservedNodeOffering (Maybe Double)
rnoFixedPrice = lens _rnoFixedPrice (\s a -> s {_rnoFixedPrice = a})

-- | The duration, in seconds, for which the offering will reserve the node.
rnoDuration :: Lens' ReservedNodeOffering (Maybe Int)
rnoDuration = lens _rnoDuration (\s a -> s {_rnoDuration = a})

instance FromXML ReservedNodeOffering where
  parseXML x =
    ReservedNodeOffering'
      <$> (x .@? "ReservedNodeOfferingType")
      <*> (x .@? "CurrencyCode")
      <*> (x .@? "ReservedNodeOfferingId")
      <*> ( x .@? "RecurringCharges" .!@ mempty
              >>= may (parseXMLList "RecurringCharge")
          )
      <*> (x .@? "OfferingType")
      <*> (x .@? "UsagePrice")
      <*> (x .@? "NodeType")
      <*> (x .@? "FixedPrice")
      <*> (x .@? "Duration")

instance Hashable ReservedNodeOffering

instance NFData ReservedNodeOffering
