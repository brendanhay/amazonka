{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.MonitoringSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.MonitoringSubscription where

import Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A monitoring subscription. This structure contains information about
-- whether additional CloudWatch metrics are enabled for a given CloudFront
-- distribution.
--
-- /See:/ 'newMonitoringSubscription' smart constructor.
data MonitoringSubscription = MonitoringSubscription'
  { -- | A subscription configuration for additional CloudWatch metrics.
    realtimeMetricsSubscriptionConfig :: Core.Maybe RealtimeMetricsSubscriptionConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MonitoringSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'realtimeMetricsSubscriptionConfig', 'monitoringSubscription_realtimeMetricsSubscriptionConfig' - A subscription configuration for additional CloudWatch metrics.
newMonitoringSubscription ::
  MonitoringSubscription
newMonitoringSubscription =
  MonitoringSubscription'
    { realtimeMetricsSubscriptionConfig =
        Core.Nothing
    }

-- | A subscription configuration for additional CloudWatch metrics.
monitoringSubscription_realtimeMetricsSubscriptionConfig :: Lens.Lens' MonitoringSubscription (Core.Maybe RealtimeMetricsSubscriptionConfig)
monitoringSubscription_realtimeMetricsSubscriptionConfig = Lens.lens (\MonitoringSubscription' {realtimeMetricsSubscriptionConfig} -> realtimeMetricsSubscriptionConfig) (\s@MonitoringSubscription' {} a -> s {realtimeMetricsSubscriptionConfig = a} :: MonitoringSubscription)

instance Core.FromXML MonitoringSubscription where
  parseXML x =
    MonitoringSubscription'
      Core.<$> (x Core..@? "RealtimeMetricsSubscriptionConfig")

instance Core.Hashable MonitoringSubscription

instance Core.NFData MonitoringSubscription

instance Core.ToXML MonitoringSubscription where
  toXML MonitoringSubscription' {..} =
    Core.mconcat
      [ "RealtimeMetricsSubscriptionConfig"
          Core.@= realtimeMetricsSubscriptionConfig
      ]
