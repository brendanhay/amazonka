{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ReservedNode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ReservedNode where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.RecurringCharge
import Network.AWS.Redshift.Types.ReservedNodeOfferingType

-- | Describes a reserved node. You can call the 'DescribeReservedNodeOfferings' API to obtain the available reserved node offerings.
--
--
--
-- /See:/ 'reservedNode' smart constructor.
data ReservedNode = ReservedNode'
  { _rnReservedNodeOfferingType ::
      !(Maybe ReservedNodeOfferingType),
    _rnState :: !(Maybe Text),
    _rnCurrencyCode :: !(Maybe Text),
    _rnStartTime :: !(Maybe ISO8601),
    _rnNodeCount :: !(Maybe Int),
    _rnReservedNodeId :: !(Maybe Text),
    _rnReservedNodeOfferingId :: !(Maybe Text),
    _rnRecurringCharges :: !(Maybe [RecurringCharge]),
    _rnOfferingType :: !(Maybe Text),
    _rnUsagePrice :: !(Maybe Double),
    _rnNodeType :: !(Maybe Text),
    _rnFixedPrice :: !(Maybe Double),
    _rnDuration :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservedNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rnReservedNodeOfferingType' -
--
-- * 'rnState' - The state of the reserved compute node. Possible Values:     * pending-payment-This reserved node has recently been purchased, and the sale has been approved, but payment has not yet been confirmed.     * active-This reserved node is owned by the caller and is available for use.     * payment-failed-Payment failed for the purchase attempt.     * retired-The reserved node is no longer available.      * exchanging-The owner is exchanging the reserved node for another reserved node.
--
-- * 'rnCurrencyCode' - The currency code for the reserved cluster.
--
-- * 'rnStartTime' - The time the reservation started. You purchase a reserved node offering for a duration. This is the start time of that duration.
--
-- * 'rnNodeCount' - The number of reserved compute nodes.
--
-- * 'rnReservedNodeId' - The unique identifier for the reservation.
--
-- * 'rnReservedNodeOfferingId' - The identifier for the reserved node offering.
--
-- * 'rnRecurringCharges' - The recurring charges for the reserved node.
--
-- * 'rnOfferingType' - The anticipated utilization of the reserved node, as defined in the reserved node offering.
--
-- * 'rnUsagePrice' - The hourly rate Amazon Redshift charges you for this reserved node.
--
-- * 'rnNodeType' - The node type of the reserved node.
--
-- * 'rnFixedPrice' - The fixed cost Amazon Redshift charges you for this reserved node.
--
-- * 'rnDuration' - The duration of the node reservation in seconds.
reservedNode ::
  ReservedNode
reservedNode =
  ReservedNode'
    { _rnReservedNodeOfferingType = Nothing,
      _rnState = Nothing,
      _rnCurrencyCode = Nothing,
      _rnStartTime = Nothing,
      _rnNodeCount = Nothing,
      _rnReservedNodeId = Nothing,
      _rnReservedNodeOfferingId = Nothing,
      _rnRecurringCharges = Nothing,
      _rnOfferingType = Nothing,
      _rnUsagePrice = Nothing,
      _rnNodeType = Nothing,
      _rnFixedPrice = Nothing,
      _rnDuration = Nothing
    }

-- |
rnReservedNodeOfferingType :: Lens' ReservedNode (Maybe ReservedNodeOfferingType)
rnReservedNodeOfferingType = lens _rnReservedNodeOfferingType (\s a -> s {_rnReservedNodeOfferingType = a})

-- | The state of the reserved compute node. Possible Values:     * pending-payment-This reserved node has recently been purchased, and the sale has been approved, but payment has not yet been confirmed.     * active-This reserved node is owned by the caller and is available for use.     * payment-failed-Payment failed for the purchase attempt.     * retired-The reserved node is no longer available.      * exchanging-The owner is exchanging the reserved node for another reserved node.
rnState :: Lens' ReservedNode (Maybe Text)
rnState = lens _rnState (\s a -> s {_rnState = a})

-- | The currency code for the reserved cluster.
rnCurrencyCode :: Lens' ReservedNode (Maybe Text)
rnCurrencyCode = lens _rnCurrencyCode (\s a -> s {_rnCurrencyCode = a})

-- | The time the reservation started. You purchase a reserved node offering for a duration. This is the start time of that duration.
rnStartTime :: Lens' ReservedNode (Maybe UTCTime)
rnStartTime = lens _rnStartTime (\s a -> s {_rnStartTime = a}) . mapping _Time

-- | The number of reserved compute nodes.
rnNodeCount :: Lens' ReservedNode (Maybe Int)
rnNodeCount = lens _rnNodeCount (\s a -> s {_rnNodeCount = a})

-- | The unique identifier for the reservation.
rnReservedNodeId :: Lens' ReservedNode (Maybe Text)
rnReservedNodeId = lens _rnReservedNodeId (\s a -> s {_rnReservedNodeId = a})

-- | The identifier for the reserved node offering.
rnReservedNodeOfferingId :: Lens' ReservedNode (Maybe Text)
rnReservedNodeOfferingId = lens _rnReservedNodeOfferingId (\s a -> s {_rnReservedNodeOfferingId = a})

-- | The recurring charges for the reserved node.
rnRecurringCharges :: Lens' ReservedNode [RecurringCharge]
rnRecurringCharges = lens _rnRecurringCharges (\s a -> s {_rnRecurringCharges = a}) . _Default . _Coerce

-- | The anticipated utilization of the reserved node, as defined in the reserved node offering.
rnOfferingType :: Lens' ReservedNode (Maybe Text)
rnOfferingType = lens _rnOfferingType (\s a -> s {_rnOfferingType = a})

-- | The hourly rate Amazon Redshift charges you for this reserved node.
rnUsagePrice :: Lens' ReservedNode (Maybe Double)
rnUsagePrice = lens _rnUsagePrice (\s a -> s {_rnUsagePrice = a})

-- | The node type of the reserved node.
rnNodeType :: Lens' ReservedNode (Maybe Text)
rnNodeType = lens _rnNodeType (\s a -> s {_rnNodeType = a})

-- | The fixed cost Amazon Redshift charges you for this reserved node.
rnFixedPrice :: Lens' ReservedNode (Maybe Double)
rnFixedPrice = lens _rnFixedPrice (\s a -> s {_rnFixedPrice = a})

-- | The duration of the node reservation in seconds.
rnDuration :: Lens' ReservedNode (Maybe Int)
rnDuration = lens _rnDuration (\s a -> s {_rnDuration = a})

instance FromXML ReservedNode where
  parseXML x =
    ReservedNode'
      <$> (x .@? "ReservedNodeOfferingType")
      <*> (x .@? "State")
      <*> (x .@? "CurrencyCode")
      <*> (x .@? "StartTime")
      <*> (x .@? "NodeCount")
      <*> (x .@? "ReservedNodeId")
      <*> (x .@? "ReservedNodeOfferingId")
      <*> ( x .@? "RecurringCharges" .!@ mempty
              >>= may (parseXMLList "RecurringCharge")
          )
      <*> (x .@? "OfferingType")
      <*> (x .@? "UsagePrice")
      <*> (x .@? "NodeType")
      <*> (x .@? "FixedPrice")
      <*> (x .@? "Duration")

instance Hashable ReservedNode

instance NFData ReservedNode
