{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.MonitoringSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.MonitoringSubscription where

import Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
--
--
--
-- /See:/ 'monitoringSubscription' smart constructor.
newtype MonitoringSubscription = MonitoringSubscription'
  { _msRealtimeMetricsSubscriptionConfig ::
      Maybe RealtimeMetricsSubscriptionConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MonitoringSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'msRealtimeMetricsSubscriptionConfig' - A subscription configuration for additional CloudWatch metrics.
monitoringSubscription ::
  MonitoringSubscription
monitoringSubscription =
  MonitoringSubscription'
    { _msRealtimeMetricsSubscriptionConfig =
        Nothing
    }

-- | A subscription configuration for additional CloudWatch metrics.
msRealtimeMetricsSubscriptionConfig :: Lens' MonitoringSubscription (Maybe RealtimeMetricsSubscriptionConfig)
msRealtimeMetricsSubscriptionConfig = lens _msRealtimeMetricsSubscriptionConfig (\s a -> s {_msRealtimeMetricsSubscriptionConfig = a})

instance FromXML MonitoringSubscription where
  parseXML x =
    MonitoringSubscription'
      <$> (x .@? "RealtimeMetricsSubscriptionConfig")

instance Hashable MonitoringSubscription

instance NFData MonitoringSubscription

instance ToXML MonitoringSubscription where
  toXML MonitoringSubscription' {..} =
    mconcat
      [ "RealtimeMetricsSubscriptionConfig"
          @= _msRealtimeMetricsSubscriptionConfig
      ]
