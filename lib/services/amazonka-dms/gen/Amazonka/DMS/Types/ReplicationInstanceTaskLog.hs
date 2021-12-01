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
-- Module      : Amazonka.DMS.Types.ReplicationInstanceTaskLog
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.ReplicationInstanceTaskLog where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains metadata for a replication instance task log.
--
-- /See:/ 'newReplicationInstanceTaskLog' smart constructor.
data ReplicationInstanceTaskLog = ReplicationInstanceTaskLog'
  { -- | The name of the replication task.
    replicationTaskName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskArn :: Prelude.Maybe Prelude.Text,
    -- | The size, in bytes, of the replication task log.
    replicationInstanceTaskLogSize :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationInstanceTaskLog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTaskName', 'replicationInstanceTaskLog_replicationTaskName' - The name of the replication task.
--
-- 'replicationTaskArn', 'replicationInstanceTaskLog_replicationTaskArn' - The Amazon Resource Name (ARN) of the replication task.
--
-- 'replicationInstanceTaskLogSize', 'replicationInstanceTaskLog_replicationInstanceTaskLogSize' - The size, in bytes, of the replication task log.
newReplicationInstanceTaskLog ::
  ReplicationInstanceTaskLog
newReplicationInstanceTaskLog =
  ReplicationInstanceTaskLog'
    { replicationTaskName =
        Prelude.Nothing,
      replicationTaskArn = Prelude.Nothing,
      replicationInstanceTaskLogSize =
        Prelude.Nothing
    }

-- | The name of the replication task.
replicationInstanceTaskLog_replicationTaskName :: Lens.Lens' ReplicationInstanceTaskLog (Prelude.Maybe Prelude.Text)
replicationInstanceTaskLog_replicationTaskName = Lens.lens (\ReplicationInstanceTaskLog' {replicationTaskName} -> replicationTaskName) (\s@ReplicationInstanceTaskLog' {} a -> s {replicationTaskName = a} :: ReplicationInstanceTaskLog)

-- | The Amazon Resource Name (ARN) of the replication task.
replicationInstanceTaskLog_replicationTaskArn :: Lens.Lens' ReplicationInstanceTaskLog (Prelude.Maybe Prelude.Text)
replicationInstanceTaskLog_replicationTaskArn = Lens.lens (\ReplicationInstanceTaskLog' {replicationTaskArn} -> replicationTaskArn) (\s@ReplicationInstanceTaskLog' {} a -> s {replicationTaskArn = a} :: ReplicationInstanceTaskLog)

-- | The size, in bytes, of the replication task log.
replicationInstanceTaskLog_replicationInstanceTaskLogSize :: Lens.Lens' ReplicationInstanceTaskLog (Prelude.Maybe Prelude.Integer)
replicationInstanceTaskLog_replicationInstanceTaskLogSize = Lens.lens (\ReplicationInstanceTaskLog' {replicationInstanceTaskLogSize} -> replicationInstanceTaskLogSize) (\s@ReplicationInstanceTaskLog' {} a -> s {replicationInstanceTaskLogSize = a} :: ReplicationInstanceTaskLog)

instance Core.FromJSON ReplicationInstanceTaskLog where
  parseJSON =
    Core.withObject
      "ReplicationInstanceTaskLog"
      ( \x ->
          ReplicationInstanceTaskLog'
            Prelude.<$> (x Core..:? "ReplicationTaskName")
            Prelude.<*> (x Core..:? "ReplicationTaskArn")
            Prelude.<*> (x Core..:? "ReplicationInstanceTaskLogSize")
      )

instance Prelude.Hashable ReplicationInstanceTaskLog where
  hashWithSalt salt' ReplicationInstanceTaskLog' {..} =
    salt'
      `Prelude.hashWithSalt` replicationInstanceTaskLogSize
      `Prelude.hashWithSalt` replicationTaskArn
      `Prelude.hashWithSalt` replicationTaskName

instance Prelude.NFData ReplicationInstanceTaskLog where
  rnf ReplicationInstanceTaskLog' {..} =
    Prelude.rnf replicationTaskName
      `Prelude.seq` Prelude.rnf replicationInstanceTaskLogSize
      `Prelude.seq` Prelude.rnf replicationTaskArn
