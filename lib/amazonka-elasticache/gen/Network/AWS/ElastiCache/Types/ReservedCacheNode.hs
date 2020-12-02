{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReservedCacheNode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReservedCacheNode where

import Network.AWS.ElastiCache.Types.RecurringCharge
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a @PurchaseReservedCacheNodesOffering@ operation.
--
--
--
-- /See:/ 'reservedCacheNode' smart constructor.
data ReservedCacheNode = ReservedCacheNode'
  { _rcnCacheNodeType ::
      !(Maybe Text),
    _rcnState :: !(Maybe Text),
    _rcnStartTime :: !(Maybe ISO8601),
    _rcnProductDescription :: !(Maybe Text),
    _rcnReservationARN :: !(Maybe Text),
    _rcnCacheNodeCount :: !(Maybe Int),
    _rcnReservedCacheNodeId :: !(Maybe Text),
    _rcnRecurringCharges :: !(Maybe [RecurringCharge]),
    _rcnOfferingType :: !(Maybe Text),
    _rcnUsagePrice :: !(Maybe Double),
    _rcnFixedPrice :: !(Maybe Double),
    _rcnDuration :: !(Maybe Int),
    _rcnReservedCacheNodesOfferingId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservedCacheNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcnCacheNodeType' - The cache node type for the reserved cache nodes. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __M6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.m6g.large@ , @cache.m6g.xlarge@ , @cache.m6g.2xlarge@ , @cache.m6g.4xlarge@ , @cache.m6g.8xlarge@ , @cache.m6g.12xlarge@ , @cache.m6g.16xlarge@  __M5 node types:__ @cache.m5.large@ , @cache.m5.xlarge@ , @cache.m5.2xlarge@ , @cache.m5.4xlarge@ , @cache.m5.12xlarge@ , @cache.m5.24xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@  __T3 node types:__ @cache.t3.micro@ , @cache.t3.small@ , @cache.t3.medium@  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.r6g.large@ , @cache.r6g.xlarge@ , @cache.r6g.2xlarge@ , @cache.r6g.4xlarge@ , @cache.r6g.8xlarge@ , @cache.r6g.12xlarge@ , @cache.r6g.16xlarge@  __R5 node types:__ @cache.r5.large@ , @cache.r5.xlarge@ , @cache.r5.2xlarge@ , @cache.r5.4xlarge@ , @cache.r5.12xlarge@ , @cache.r5.24xlarge@  __R4 node types:__ @cache.r4.large@ , @cache.r4.xlarge@ , @cache.r4.2xlarge@ , @cache.r4.4xlarge@ , @cache.r4.8xlarge@ , @cache.r4.16xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@  __Additional node type info__      * All current generation instance types are created in Amazon VPC by default.     * Redis append-only files (AOF) are not supported for T1 or T2 instances.     * Redis Multi-AZ with automatic failover is not supported on T1 instances.     * Redis configuration variables @appendonly@ and @appendfsync@ are not supported on Redis version 2.8.22 and later.
--
-- * 'rcnState' - The state of the reserved cache node.
--
-- * 'rcnStartTime' - The time the reservation started.
--
-- * 'rcnProductDescription' - The description of the reserved cache node.
--
-- * 'rcnReservationARN' - The Amazon Resource Name (ARN) of the reserved cache node. Example: @arn:aws:elasticache:us-east-1:123456789012:reserved-instance:ri-2017-03-27-08-33-25-582@
--
-- * 'rcnCacheNodeCount' - The number of cache nodes that have been reserved.
--
-- * 'rcnReservedCacheNodeId' - The unique identifier for the reservation.
--
-- * 'rcnRecurringCharges' - The recurring price charged to run this reserved cache node.
--
-- * 'rcnOfferingType' - The offering type of this reserved cache node.
--
-- * 'rcnUsagePrice' - The hourly price charged for this reserved cache node.
--
-- * 'rcnFixedPrice' - The fixed price charged for this reserved cache node.
--
-- * 'rcnDuration' - The duration of the reservation in seconds.
--
-- * 'rcnReservedCacheNodesOfferingId' - The offering identifier.
reservedCacheNode ::
  ReservedCacheNode
reservedCacheNode =
  ReservedCacheNode'
    { _rcnCacheNodeType = Nothing,
      _rcnState = Nothing,
      _rcnStartTime = Nothing,
      _rcnProductDescription = Nothing,
      _rcnReservationARN = Nothing,
      _rcnCacheNodeCount = Nothing,
      _rcnReservedCacheNodeId = Nothing,
      _rcnRecurringCharges = Nothing,
      _rcnOfferingType = Nothing,
      _rcnUsagePrice = Nothing,
      _rcnFixedPrice = Nothing,
      _rcnDuration = Nothing,
      _rcnReservedCacheNodesOfferingId = Nothing
    }

-- | The cache node type for the reserved cache nodes. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __M6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.m6g.large@ , @cache.m6g.xlarge@ , @cache.m6g.2xlarge@ , @cache.m6g.4xlarge@ , @cache.m6g.8xlarge@ , @cache.m6g.12xlarge@ , @cache.m6g.16xlarge@  __M5 node types:__ @cache.m5.large@ , @cache.m5.xlarge@ , @cache.m5.2xlarge@ , @cache.m5.4xlarge@ , @cache.m5.12xlarge@ , @cache.m5.24xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@  __T3 node types:__ @cache.t3.micro@ , @cache.t3.small@ , @cache.t3.medium@  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.r6g.large@ , @cache.r6g.xlarge@ , @cache.r6g.2xlarge@ , @cache.r6g.4xlarge@ , @cache.r6g.8xlarge@ , @cache.r6g.12xlarge@ , @cache.r6g.16xlarge@  __R5 node types:__ @cache.r5.large@ , @cache.r5.xlarge@ , @cache.r5.2xlarge@ , @cache.r5.4xlarge@ , @cache.r5.12xlarge@ , @cache.r5.24xlarge@  __R4 node types:__ @cache.r4.large@ , @cache.r4.xlarge@ , @cache.r4.2xlarge@ , @cache.r4.4xlarge@ , @cache.r4.8xlarge@ , @cache.r4.16xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@  __Additional node type info__      * All current generation instance types are created in Amazon VPC by default.     * Redis append-only files (AOF) are not supported for T1 or T2 instances.     * Redis Multi-AZ with automatic failover is not supported on T1 instances.     * Redis configuration variables @appendonly@ and @appendfsync@ are not supported on Redis version 2.8.22 and later.
rcnCacheNodeType :: Lens' ReservedCacheNode (Maybe Text)
rcnCacheNodeType = lens _rcnCacheNodeType (\s a -> s {_rcnCacheNodeType = a})

-- | The state of the reserved cache node.
rcnState :: Lens' ReservedCacheNode (Maybe Text)
rcnState = lens _rcnState (\s a -> s {_rcnState = a})

-- | The time the reservation started.
rcnStartTime :: Lens' ReservedCacheNode (Maybe UTCTime)
rcnStartTime = lens _rcnStartTime (\s a -> s {_rcnStartTime = a}) . mapping _Time

-- | The description of the reserved cache node.
rcnProductDescription :: Lens' ReservedCacheNode (Maybe Text)
rcnProductDescription = lens _rcnProductDescription (\s a -> s {_rcnProductDescription = a})

-- | The Amazon Resource Name (ARN) of the reserved cache node. Example: @arn:aws:elasticache:us-east-1:123456789012:reserved-instance:ri-2017-03-27-08-33-25-582@
rcnReservationARN :: Lens' ReservedCacheNode (Maybe Text)
rcnReservationARN = lens _rcnReservationARN (\s a -> s {_rcnReservationARN = a})

-- | The number of cache nodes that have been reserved.
rcnCacheNodeCount :: Lens' ReservedCacheNode (Maybe Int)
rcnCacheNodeCount = lens _rcnCacheNodeCount (\s a -> s {_rcnCacheNodeCount = a})

-- | The unique identifier for the reservation.
rcnReservedCacheNodeId :: Lens' ReservedCacheNode (Maybe Text)
rcnReservedCacheNodeId = lens _rcnReservedCacheNodeId (\s a -> s {_rcnReservedCacheNodeId = a})

-- | The recurring price charged to run this reserved cache node.
rcnRecurringCharges :: Lens' ReservedCacheNode [RecurringCharge]
rcnRecurringCharges = lens _rcnRecurringCharges (\s a -> s {_rcnRecurringCharges = a}) . _Default . _Coerce

-- | The offering type of this reserved cache node.
rcnOfferingType :: Lens' ReservedCacheNode (Maybe Text)
rcnOfferingType = lens _rcnOfferingType (\s a -> s {_rcnOfferingType = a})

-- | The hourly price charged for this reserved cache node.
rcnUsagePrice :: Lens' ReservedCacheNode (Maybe Double)
rcnUsagePrice = lens _rcnUsagePrice (\s a -> s {_rcnUsagePrice = a})

-- | The fixed price charged for this reserved cache node.
rcnFixedPrice :: Lens' ReservedCacheNode (Maybe Double)
rcnFixedPrice = lens _rcnFixedPrice (\s a -> s {_rcnFixedPrice = a})

-- | The duration of the reservation in seconds.
rcnDuration :: Lens' ReservedCacheNode (Maybe Int)
rcnDuration = lens _rcnDuration (\s a -> s {_rcnDuration = a})

-- | The offering identifier.
rcnReservedCacheNodesOfferingId :: Lens' ReservedCacheNode (Maybe Text)
rcnReservedCacheNodesOfferingId = lens _rcnReservedCacheNodesOfferingId (\s a -> s {_rcnReservedCacheNodesOfferingId = a})

instance FromXML ReservedCacheNode where
  parseXML x =
    ReservedCacheNode'
      <$> (x .@? "CacheNodeType")
      <*> (x .@? "State")
      <*> (x .@? "StartTime")
      <*> (x .@? "ProductDescription")
      <*> (x .@? "ReservationARN")
      <*> (x .@? "CacheNodeCount")
      <*> (x .@? "ReservedCacheNodeId")
      <*> ( x .@? "RecurringCharges" .!@ mempty
              >>= may (parseXMLList "RecurringCharge")
          )
      <*> (x .@? "OfferingType")
      <*> (x .@? "UsagePrice")
      <*> (x .@? "FixedPrice")
      <*> (x .@? "Duration")
      <*> (x .@? "ReservedCacheNodesOfferingId")

instance Hashable ReservedCacheNode

instance NFData ReservedCacheNode
