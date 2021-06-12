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
-- Module      : Network.AWS.S3.Types.Metrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Metrics where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.MetricsStatus
import Network.AWS.S3.Types.ReplicationTimeValue

-- | A container specifying replication metrics-related settings enabling
-- replication metrics and events.
--
-- /See:/ 'newMetrics' smart constructor.
data Metrics = Metrics'
  { -- | A container specifying the time threshold for emitting the
    -- @s3:Replication:OperationMissedThreshold@ event.
    eventThreshold :: Core.Maybe ReplicationTimeValue,
    -- | Specifies whether the replication metrics are enabled.
    status :: MetricsStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Metrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventThreshold', 'metrics_eventThreshold' - A container specifying the time threshold for emitting the
-- @s3:Replication:OperationMissedThreshold@ event.
--
-- 'status', 'metrics_status' - Specifies whether the replication metrics are enabled.
newMetrics ::
  -- | 'status'
  MetricsStatus ->
  Metrics
newMetrics pStatus_ =
  Metrics'
    { eventThreshold = Core.Nothing,
      status = pStatus_
    }

-- | A container specifying the time threshold for emitting the
-- @s3:Replication:OperationMissedThreshold@ event.
metrics_eventThreshold :: Lens.Lens' Metrics (Core.Maybe ReplicationTimeValue)
metrics_eventThreshold = Lens.lens (\Metrics' {eventThreshold} -> eventThreshold) (\s@Metrics' {} a -> s {eventThreshold = a} :: Metrics)

-- | Specifies whether the replication metrics are enabled.
metrics_status :: Lens.Lens' Metrics MetricsStatus
metrics_status = Lens.lens (\Metrics' {status} -> status) (\s@Metrics' {} a -> s {status = a} :: Metrics)

instance Core.FromXML Metrics where
  parseXML x =
    Metrics'
      Core.<$> (x Core..@? "EventThreshold")
      Core.<*> (x Core..@ "Status")

instance Core.Hashable Metrics

instance Core.NFData Metrics

instance Core.ToXML Metrics where
  toXML Metrics' {..} =
    Core.mconcat
      [ "EventThreshold" Core.@= eventThreshold,
        "Status" Core.@= status
      ]
