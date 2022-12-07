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
-- Module      : Amazonka.CloudWatchEvents.Types.ReplicationConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.ReplicationConfig where

import Amazonka.CloudWatchEvents.Types.ReplicationState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Endpoints can replicate all events to the secondary Region.
--
-- /See:/ 'newReplicationConfig' smart constructor.
data ReplicationConfig = ReplicationConfig'
  { -- | The state of event replication.
    state :: Prelude.Maybe ReplicationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'replicationConfig_state' - The state of event replication.
newReplicationConfig ::
  ReplicationConfig
newReplicationConfig =
  ReplicationConfig' {state = Prelude.Nothing}

-- | The state of event replication.
replicationConfig_state :: Lens.Lens' ReplicationConfig (Prelude.Maybe ReplicationState)
replicationConfig_state = Lens.lens (\ReplicationConfig' {state} -> state) (\s@ReplicationConfig' {} a -> s {state = a} :: ReplicationConfig)

instance Data.FromJSON ReplicationConfig where
  parseJSON =
    Data.withObject
      "ReplicationConfig"
      ( \x ->
          ReplicationConfig' Prelude.<$> (x Data..:? "State")
      )

instance Prelude.Hashable ReplicationConfig where
  hashWithSalt _salt ReplicationConfig' {..} =
    _salt `Prelude.hashWithSalt` state

instance Prelude.NFData ReplicationConfig where
  rnf ReplicationConfig' {..} = Prelude.rnf state

instance Data.ToJSON ReplicationConfig where
  toJSON ReplicationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [("State" Data..=) Prelude.<$> state]
      )
