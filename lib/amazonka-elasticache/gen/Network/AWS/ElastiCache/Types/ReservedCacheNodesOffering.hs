{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReservedCacheNodesOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReservedCacheNodesOffering where

import Network.AWS.ElastiCache.Types.RecurringCharge
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes all of the attributes of a reserved cache node offering.
--
--
--
-- /See:/ 'reservedCacheNodesOffering' smart constructor.
data ReservedCacheNodesOffering = ReservedCacheNodesOffering'
  { _rcnoCacheNodeType ::
      !(Maybe Text),
    _rcnoProductDescription ::
      !(Maybe Text),
    _rcnoRecurringCharges ::
      !(Maybe [RecurringCharge]),
    _rcnoOfferingType :: !(Maybe Text),
    _rcnoUsagePrice :: !(Maybe Double),
    _rcnoFixedPrice :: !(Maybe Double),
    _rcnoDuration :: !(Maybe Int),
    _rcnoReservedCacheNodesOfferingId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservedCacheNodesOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcnoCacheNodeType' - The cache node type for the reserved cache node. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __M6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.m6g.large@ , @cache.m6g.xlarge@ , @cache.m6g.2xlarge@ , @cache.m6g.4xlarge@ , @cache.m6g.8xlarge@ , @cache.m6g.12xlarge@ , @cache.m6g.16xlarge@  __M5 node types:__ @cache.m5.large@ , @cache.m5.xlarge@ , @cache.m5.2xlarge@ , @cache.m5.4xlarge@ , @cache.m5.12xlarge@ , @cache.m5.24xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@  __T3 node types:__ @cache.t3.micro@ , @cache.t3.small@ , @cache.t3.medium@  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.r6g.large@ , @cache.r6g.xlarge@ , @cache.r6g.2xlarge@ , @cache.r6g.4xlarge@ , @cache.r6g.8xlarge@ , @cache.r6g.12xlarge@ , @cache.r6g.16xlarge@  __R5 node types:__ @cache.r5.large@ , @cache.r5.xlarge@ , @cache.r5.2xlarge@ , @cache.r5.4xlarge@ , @cache.r5.12xlarge@ , @cache.r5.24xlarge@  __R4 node types:__ @cache.r4.large@ , @cache.r4.xlarge@ , @cache.r4.2xlarge@ , @cache.r4.4xlarge@ , @cache.r4.8xlarge@ , @cache.r4.16xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@  __Additional node type info__      * All current generation instance types are created in Amazon VPC by default.     * Redis append-only files (AOF) are not supported for T1 or T2 instances.     * Redis Multi-AZ with automatic failover is not supported on T1 instances.     * Redis configuration variables @appendonly@ and @appendfsync@ are not supported on Redis version 2.8.22 and later.
--
-- * 'rcnoProductDescription' - The cache engine used by the offering.
--
-- * 'rcnoRecurringCharges' - The recurring price charged to run this reserved cache node.
--
-- * 'rcnoOfferingType' - The offering type.
--
-- * 'rcnoUsagePrice' - The hourly price charged for this offering.
--
-- * 'rcnoFixedPrice' - The fixed price charged for this offering.
--
-- * 'rcnoDuration' - The duration of the offering. in seconds.
--
-- * 'rcnoReservedCacheNodesOfferingId' - A unique identifier for the reserved cache node offering.
reservedCacheNodesOffering ::
  ReservedCacheNodesOffering
reservedCacheNodesOffering =
  ReservedCacheNodesOffering'
    { _rcnoCacheNodeType = Nothing,
      _rcnoProductDescription = Nothing,
      _rcnoRecurringCharges = Nothing,
      _rcnoOfferingType = Nothing,
      _rcnoUsagePrice = Nothing,
      _rcnoFixedPrice = Nothing,
      _rcnoDuration = Nothing,
      _rcnoReservedCacheNodesOfferingId = Nothing
    }

-- | The cache node type for the reserved cache node. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __M6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.m6g.large@ , @cache.m6g.xlarge@ , @cache.m6g.2xlarge@ , @cache.m6g.4xlarge@ , @cache.m6g.8xlarge@ , @cache.m6g.12xlarge@ , @cache.m6g.16xlarge@  __M5 node types:__ @cache.m5.large@ , @cache.m5.xlarge@ , @cache.m5.2xlarge@ , @cache.m5.4xlarge@ , @cache.m5.12xlarge@ , @cache.m5.24xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@  __T3 node types:__ @cache.t3.micro@ , @cache.t3.small@ , @cache.t3.medium@  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.r6g.large@ , @cache.r6g.xlarge@ , @cache.r6g.2xlarge@ , @cache.r6g.4xlarge@ , @cache.r6g.8xlarge@ , @cache.r6g.12xlarge@ , @cache.r6g.16xlarge@  __R5 node types:__ @cache.r5.large@ , @cache.r5.xlarge@ , @cache.r5.2xlarge@ , @cache.r5.4xlarge@ , @cache.r5.12xlarge@ , @cache.r5.24xlarge@  __R4 node types:__ @cache.r4.large@ , @cache.r4.xlarge@ , @cache.r4.2xlarge@ , @cache.r4.4xlarge@ , @cache.r4.8xlarge@ , @cache.r4.16xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@  __Additional node type info__      * All current generation instance types are created in Amazon VPC by default.     * Redis append-only files (AOF) are not supported for T1 or T2 instances.     * Redis Multi-AZ with automatic failover is not supported on T1 instances.     * Redis configuration variables @appendonly@ and @appendfsync@ are not supported on Redis version 2.8.22 and later.
rcnoCacheNodeType :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoCacheNodeType = lens _rcnoCacheNodeType (\s a -> s {_rcnoCacheNodeType = a})

-- | The cache engine used by the offering.
rcnoProductDescription :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoProductDescription = lens _rcnoProductDescription (\s a -> s {_rcnoProductDescription = a})

-- | The recurring price charged to run this reserved cache node.
rcnoRecurringCharges :: Lens' ReservedCacheNodesOffering [RecurringCharge]
rcnoRecurringCharges = lens _rcnoRecurringCharges (\s a -> s {_rcnoRecurringCharges = a}) . _Default . _Coerce

-- | The offering type.
rcnoOfferingType :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoOfferingType = lens _rcnoOfferingType (\s a -> s {_rcnoOfferingType = a})

-- | The hourly price charged for this offering.
rcnoUsagePrice :: Lens' ReservedCacheNodesOffering (Maybe Double)
rcnoUsagePrice = lens _rcnoUsagePrice (\s a -> s {_rcnoUsagePrice = a})

-- | The fixed price charged for this offering.
rcnoFixedPrice :: Lens' ReservedCacheNodesOffering (Maybe Double)
rcnoFixedPrice = lens _rcnoFixedPrice (\s a -> s {_rcnoFixedPrice = a})

-- | The duration of the offering. in seconds.
rcnoDuration :: Lens' ReservedCacheNodesOffering (Maybe Int)
rcnoDuration = lens _rcnoDuration (\s a -> s {_rcnoDuration = a})

-- | A unique identifier for the reserved cache node offering.
rcnoReservedCacheNodesOfferingId :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoReservedCacheNodesOfferingId = lens _rcnoReservedCacheNodesOfferingId (\s a -> s {_rcnoReservedCacheNodesOfferingId = a})

instance FromXML ReservedCacheNodesOffering where
  parseXML x =
    ReservedCacheNodesOffering'
      <$> (x .@? "CacheNodeType")
      <*> (x .@? "ProductDescription")
      <*> ( x .@? "RecurringCharges" .!@ mempty
              >>= may (parseXMLList "RecurringCharge")
          )
      <*> (x .@? "OfferingType")
      <*> (x .@? "UsagePrice")
      <*> (x .@? "FixedPrice")
      <*> (x .@? "Duration")
      <*> (x .@? "ReservedCacheNodesOfferingId")

instance Hashable ReservedCacheNodesOffering

instance NFData ReservedCacheNodesOffering
