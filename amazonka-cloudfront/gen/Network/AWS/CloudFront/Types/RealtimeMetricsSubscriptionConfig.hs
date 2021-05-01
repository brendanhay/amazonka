{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionConfig where

import Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A subscription configuration for additional CloudWatch metrics.
--
-- /See:/ 'newRealtimeMetricsSubscriptionConfig' smart constructor.
data RealtimeMetricsSubscriptionConfig = RealtimeMetricsSubscriptionConfig'
  { -- | A flag that indicates whether additional CloudWatch metrics are enabled
    -- for a given CloudFront distribution.
    realtimeMetricsSubscriptionStatus :: RealtimeMetricsSubscriptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RealtimeMetricsSubscriptionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'realtimeMetricsSubscriptionStatus', 'realtimeMetricsSubscriptionConfig_realtimeMetricsSubscriptionStatus' - A flag that indicates whether additional CloudWatch metrics are enabled
-- for a given CloudFront distribution.
newRealtimeMetricsSubscriptionConfig ::
  -- | 'realtimeMetricsSubscriptionStatus'
  RealtimeMetricsSubscriptionStatus ->
  RealtimeMetricsSubscriptionConfig
newRealtimeMetricsSubscriptionConfig
  pRealtimeMetricsSubscriptionStatus_ =
    RealtimeMetricsSubscriptionConfig'
      { realtimeMetricsSubscriptionStatus =
          pRealtimeMetricsSubscriptionStatus_
      }

-- | A flag that indicates whether additional CloudWatch metrics are enabled
-- for a given CloudFront distribution.
realtimeMetricsSubscriptionConfig_realtimeMetricsSubscriptionStatus :: Lens.Lens' RealtimeMetricsSubscriptionConfig RealtimeMetricsSubscriptionStatus
realtimeMetricsSubscriptionConfig_realtimeMetricsSubscriptionStatus = Lens.lens (\RealtimeMetricsSubscriptionConfig' {realtimeMetricsSubscriptionStatus} -> realtimeMetricsSubscriptionStatus) (\s@RealtimeMetricsSubscriptionConfig' {} a -> s {realtimeMetricsSubscriptionStatus = a} :: RealtimeMetricsSubscriptionConfig)

instance
  Prelude.FromXML
    RealtimeMetricsSubscriptionConfig
  where
  parseXML x =
    RealtimeMetricsSubscriptionConfig'
      Prelude.<$> (x Prelude..@ "RealtimeMetricsSubscriptionStatus")

instance
  Prelude.Hashable
    RealtimeMetricsSubscriptionConfig

instance
  Prelude.NFData
    RealtimeMetricsSubscriptionConfig

instance
  Prelude.ToXML
    RealtimeMetricsSubscriptionConfig
  where
  toXML RealtimeMetricsSubscriptionConfig' {..} =
    Prelude.mconcat
      [ "RealtimeMetricsSubscriptionStatus"
          Prelude.@= realtimeMetricsSubscriptionStatus
      ]
