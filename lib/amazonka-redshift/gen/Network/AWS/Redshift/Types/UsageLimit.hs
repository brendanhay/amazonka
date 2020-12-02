{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.UsageLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.UsageLimit where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag
import Network.AWS.Redshift.Types.UsageLimitBreachAction
import Network.AWS.Redshift.Types.UsageLimitFeatureType
import Network.AWS.Redshift.Types.UsageLimitLimitType
import Network.AWS.Redshift.Types.UsageLimitPeriod

-- | Describes a usage limit object for a cluster.
--
--
--
-- /See:/ 'usageLimit' smart constructor.
data UsageLimit = UsageLimit'
  { _ulAmount :: !(Maybe Integer),
    _ulLimitType :: !(Maybe UsageLimitLimitType),
    _ulUsageLimitId :: !(Maybe Text),
    _ulPeriod :: !(Maybe UsageLimitPeriod),
    _ulClusterIdentifier :: !(Maybe Text),
    _ulBreachAction :: !(Maybe UsageLimitBreachAction),
    _ulFeatureType :: !(Maybe UsageLimitFeatureType),
    _ulTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UsageLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ulAmount' - The limit amount. If time-based, this amount is in minutes. If data-based, this amount is in terabytes (TB).
--
-- * 'ulLimitType' - The type of limit. Depending on the feature type, this can be based on a time duration or data size.
--
-- * 'ulUsageLimitId' - The identifier of the usage limit.
--
-- * 'ulPeriod' - The time period that the amount applies to. A @weekly@ period begins on Sunday. The default is @monthly@ .
--
-- * 'ulClusterIdentifier' - The identifier of the cluster with a usage limit.
--
-- * 'ulBreachAction' - The action that Amazon Redshift takes when the limit is reached. Possible values are:      * __log__ - To log an event in a system table. The default is log.     * __emit-metric__ - To emit CloudWatch metrics.     * __disable__ - To disable the feature until the next usage period begins.
--
-- * 'ulFeatureType' - The Amazon Redshift feature to which the limit applies.
--
-- * 'ulTags' - A list of tag instances.
usageLimit ::
  UsageLimit
usageLimit =
  UsageLimit'
    { _ulAmount = Nothing,
      _ulLimitType = Nothing,
      _ulUsageLimitId = Nothing,
      _ulPeriod = Nothing,
      _ulClusterIdentifier = Nothing,
      _ulBreachAction = Nothing,
      _ulFeatureType = Nothing,
      _ulTags = Nothing
    }

-- | The limit amount. If time-based, this amount is in minutes. If data-based, this amount is in terabytes (TB).
ulAmount :: Lens' UsageLimit (Maybe Integer)
ulAmount = lens _ulAmount (\s a -> s {_ulAmount = a})

-- | The type of limit. Depending on the feature type, this can be based on a time duration or data size.
ulLimitType :: Lens' UsageLimit (Maybe UsageLimitLimitType)
ulLimitType = lens _ulLimitType (\s a -> s {_ulLimitType = a})

-- | The identifier of the usage limit.
ulUsageLimitId :: Lens' UsageLimit (Maybe Text)
ulUsageLimitId = lens _ulUsageLimitId (\s a -> s {_ulUsageLimitId = a})

-- | The time period that the amount applies to. A @weekly@ period begins on Sunday. The default is @monthly@ .
ulPeriod :: Lens' UsageLimit (Maybe UsageLimitPeriod)
ulPeriod = lens _ulPeriod (\s a -> s {_ulPeriod = a})

-- | The identifier of the cluster with a usage limit.
ulClusterIdentifier :: Lens' UsageLimit (Maybe Text)
ulClusterIdentifier = lens _ulClusterIdentifier (\s a -> s {_ulClusterIdentifier = a})

-- | The action that Amazon Redshift takes when the limit is reached. Possible values are:      * __log__ - To log an event in a system table. The default is log.     * __emit-metric__ - To emit CloudWatch metrics.     * __disable__ - To disable the feature until the next usage period begins.
ulBreachAction :: Lens' UsageLimit (Maybe UsageLimitBreachAction)
ulBreachAction = lens _ulBreachAction (\s a -> s {_ulBreachAction = a})

-- | The Amazon Redshift feature to which the limit applies.
ulFeatureType :: Lens' UsageLimit (Maybe UsageLimitFeatureType)
ulFeatureType = lens _ulFeatureType (\s a -> s {_ulFeatureType = a})

-- | A list of tag instances.
ulTags :: Lens' UsageLimit [Tag]
ulTags = lens _ulTags (\s a -> s {_ulTags = a}) . _Default . _Coerce

instance FromXML UsageLimit where
  parseXML x =
    UsageLimit'
      <$> (x .@? "Amount")
      <*> (x .@? "LimitType")
      <*> (x .@? "UsageLimitId")
      <*> (x .@? "Period")
      <*> (x .@? "ClusterIdentifier")
      <*> (x .@? "BreachAction")
      <*> (x .@? "FeatureType")
      <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable UsageLimit

instance NFData UsageLimit
