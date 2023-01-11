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
-- Module      : Amazonka.S3.Types.ReplicationTime
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ReplicationTime where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ReplicationTimeStatus
import Amazonka.S3.Types.ReplicationTimeValue

-- | A container specifying S3 Replication Time Control (S3 RTC) related
-- information, including whether S3 RTC is enabled and the time when all
-- objects and operations on objects must be replicated. Must be specified
-- together with a @Metrics@ block.
--
-- /See:/ 'newReplicationTime' smart constructor.
data ReplicationTime = ReplicationTime'
  { -- | Specifies whether the replication time is enabled.
    status :: ReplicationTimeStatus,
    -- | A container specifying the time by which replication should be complete
    -- for all objects and operations on objects.
    time :: ReplicationTimeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'replicationTime_status' - Specifies whether the replication time is enabled.
--
-- 'time', 'replicationTime_time' - A container specifying the time by which replication should be complete
-- for all objects and operations on objects.
newReplicationTime ::
  -- | 'status'
  ReplicationTimeStatus ->
  -- | 'time'
  ReplicationTimeValue ->
  ReplicationTime
newReplicationTime pStatus_ pTime_ =
  ReplicationTime' {status = pStatus_, time = pTime_}

-- | Specifies whether the replication time is enabled.
replicationTime_status :: Lens.Lens' ReplicationTime ReplicationTimeStatus
replicationTime_status = Lens.lens (\ReplicationTime' {status} -> status) (\s@ReplicationTime' {} a -> s {status = a} :: ReplicationTime)

-- | A container specifying the time by which replication should be complete
-- for all objects and operations on objects.
replicationTime_time :: Lens.Lens' ReplicationTime ReplicationTimeValue
replicationTime_time = Lens.lens (\ReplicationTime' {time} -> time) (\s@ReplicationTime' {} a -> s {time = a} :: ReplicationTime)

instance Data.FromXML ReplicationTime where
  parseXML x =
    ReplicationTime'
      Prelude.<$> (x Data..@ "Status") Prelude.<*> (x Data..@ "Time")

instance Prelude.Hashable ReplicationTime where
  hashWithSalt _salt ReplicationTime' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` time

instance Prelude.NFData ReplicationTime where
  rnf ReplicationTime' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf time

instance Data.ToXML ReplicationTime where
  toXML ReplicationTime' {..} =
    Prelude.mconcat
      ["Status" Data.@= status, "Time" Data.@= time]
