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
-- Module      : Network.AWS.S3.Types.ReplicationTimeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationTimeValue where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | A container specifying the time value for S3 Replication Time Control
-- (S3 RTC) and replication metrics @EventThreshold@.
--
-- /See:/ 'newReplicationTimeValue' smart constructor.
data ReplicationTimeValue = ReplicationTimeValue'
  { -- | Contains an integer specifying time in minutes.
    --
    -- Valid values: 15 minutes.
    minutes :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReplicationTimeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minutes', 'replicationTimeValue_minutes' - Contains an integer specifying time in minutes.
--
-- Valid values: 15 minutes.
newReplicationTimeValue ::
  ReplicationTimeValue
newReplicationTimeValue =
  ReplicationTimeValue' {minutes = Prelude.Nothing}

-- | Contains an integer specifying time in minutes.
--
-- Valid values: 15 minutes.
replicationTimeValue_minutes :: Lens.Lens' ReplicationTimeValue (Prelude.Maybe Prelude.Int)
replicationTimeValue_minutes = Lens.lens (\ReplicationTimeValue' {minutes} -> minutes) (\s@ReplicationTimeValue' {} a -> s {minutes = a} :: ReplicationTimeValue)

instance Prelude.FromXML ReplicationTimeValue where
  parseXML x =
    ReplicationTimeValue'
      Prelude.<$> (x Prelude..@? "Minutes")

instance Prelude.Hashable ReplicationTimeValue

instance Prelude.NFData ReplicationTimeValue

instance Prelude.ToXML ReplicationTimeValue where
  toXML ReplicationTimeValue' {..} =
    Prelude.mconcat ["Minutes" Prelude.@= minutes]
