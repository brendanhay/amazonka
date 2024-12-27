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
-- Module      : Amazonka.S3.Types.Metrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.Metrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.MetricsStatus
import Amazonka.S3.Types.ReplicationTimeValue

-- | A container specifying replication metrics-related settings enabling
-- replication metrics and events.
--
-- /See:/ 'newMetrics' smart constructor.
data Metrics = Metrics'
  { -- | A container specifying the time threshold for emitting the
    -- @s3:Replication:OperationMissedThreshold@ event.
    eventThreshold :: Prelude.Maybe ReplicationTimeValue,
    -- | Specifies whether the replication metrics are enabled.
    status :: MetricsStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { eventThreshold = Prelude.Nothing,
      status = pStatus_
    }

-- | A container specifying the time threshold for emitting the
-- @s3:Replication:OperationMissedThreshold@ event.
metrics_eventThreshold :: Lens.Lens' Metrics (Prelude.Maybe ReplicationTimeValue)
metrics_eventThreshold = Lens.lens (\Metrics' {eventThreshold} -> eventThreshold) (\s@Metrics' {} a -> s {eventThreshold = a} :: Metrics)

-- | Specifies whether the replication metrics are enabled.
metrics_status :: Lens.Lens' Metrics MetricsStatus
metrics_status = Lens.lens (\Metrics' {status} -> status) (\s@Metrics' {} a -> s {status = a} :: Metrics)

instance Data.FromXML Metrics where
  parseXML x =
    Metrics'
      Prelude.<$> (x Data..@? "EventThreshold")
      Prelude.<*> (x Data..@ "Status")

instance Prelude.Hashable Metrics where
  hashWithSalt _salt Metrics' {..} =
    _salt
      `Prelude.hashWithSalt` eventThreshold
      `Prelude.hashWithSalt` status

instance Prelude.NFData Metrics where
  rnf Metrics' {..} =
    Prelude.rnf eventThreshold `Prelude.seq`
      Prelude.rnf status

instance Data.ToXML Metrics where
  toXML Metrics' {..} =
    Prelude.mconcat
      [ "EventThreshold" Data.@= eventThreshold,
        "Status" Data.@= status
      ]
