{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionConfig where

import Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A subscription configuration for additional CloudWatch metrics.
--
--
--
-- /See:/ 'realtimeMetricsSubscriptionConfig' smart constructor.
newtype RealtimeMetricsSubscriptionConfig = RealtimeMetricsSubscriptionConfig'
  { _rmscRealtimeMetricsSubscriptionStatus ::
      RealtimeMetricsSubscriptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RealtimeMetricsSubscriptionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmscRealtimeMetricsSubscriptionStatus' - A flag that indicates whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
realtimeMetricsSubscriptionConfig ::
  -- | 'rmscRealtimeMetricsSubscriptionStatus'
  RealtimeMetricsSubscriptionStatus ->
  RealtimeMetricsSubscriptionConfig
realtimeMetricsSubscriptionConfig
  pRealtimeMetricsSubscriptionStatus_ =
    RealtimeMetricsSubscriptionConfig'
      { _rmscRealtimeMetricsSubscriptionStatus =
          pRealtimeMetricsSubscriptionStatus_
      }

-- | A flag that indicates whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
rmscRealtimeMetricsSubscriptionStatus :: Lens' RealtimeMetricsSubscriptionConfig RealtimeMetricsSubscriptionStatus
rmscRealtimeMetricsSubscriptionStatus = lens _rmscRealtimeMetricsSubscriptionStatus (\s a -> s {_rmscRealtimeMetricsSubscriptionStatus = a})

instance FromXML RealtimeMetricsSubscriptionConfig where
  parseXML x =
    RealtimeMetricsSubscriptionConfig'
      <$> (x .@ "RealtimeMetricsSubscriptionStatus")

instance Hashable RealtimeMetricsSubscriptionConfig

instance NFData RealtimeMetricsSubscriptionConfig

instance ToXML RealtimeMetricsSubscriptionConfig where
  toXML RealtimeMetricsSubscriptionConfig' {..} =
    mconcat
      [ "RealtimeMetricsSubscriptionStatus"
          @= _rmscRealtimeMetricsSubscriptionStatus
      ]
