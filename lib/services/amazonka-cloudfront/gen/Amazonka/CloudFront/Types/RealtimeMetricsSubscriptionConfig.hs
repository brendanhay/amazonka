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
-- Module      : Amazonka.CloudFront.Types.RealtimeMetricsSubscriptionConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.RealtimeMetricsSubscriptionConfig where

import Amazonka.CloudFront.Types.RealtimeMetricsSubscriptionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A subscription configuration for additional CloudWatch metrics.
--
-- /See:/ 'newRealtimeMetricsSubscriptionConfig' smart constructor.
data RealtimeMetricsSubscriptionConfig = RealtimeMetricsSubscriptionConfig'
  { -- | A flag that indicates whether additional CloudWatch metrics are enabled
    -- for a given CloudFront distribution.
    realtimeMetricsSubscriptionStatus :: RealtimeMetricsSubscriptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Data.FromXML
    RealtimeMetricsSubscriptionConfig
  where
  parseXML x =
    RealtimeMetricsSubscriptionConfig'
      Prelude.<$> (x Data..@ "RealtimeMetricsSubscriptionStatus")

instance
  Prelude.Hashable
    RealtimeMetricsSubscriptionConfig
  where
  hashWithSalt
    _salt
    RealtimeMetricsSubscriptionConfig' {..} =
      _salt
        `Prelude.hashWithSalt` realtimeMetricsSubscriptionStatus

instance
  Prelude.NFData
    RealtimeMetricsSubscriptionConfig
  where
  rnf RealtimeMetricsSubscriptionConfig' {..} =
    Prelude.rnf realtimeMetricsSubscriptionStatus

instance Data.ToXML RealtimeMetricsSubscriptionConfig where
  toXML RealtimeMetricsSubscriptionConfig' {..} =
    Prelude.mconcat
      [ "RealtimeMetricsSubscriptionStatus"
          Data.@= realtimeMetricsSubscriptionStatus
      ]
