{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationInstanceTaskLog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationInstanceTaskLog
  ( ReplicationInstanceTaskLog (..),

    -- * Smart constructor
    mkReplicationInstanceTaskLog,

    -- * Lenses
    ritlReplicationInstanceTaskLogSize,
    ritlReplicationTaskArn,
    ritlReplicationTaskName,
  )
where

import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains metadata for a replication instance task log.
--
-- /See:/ 'mkReplicationInstanceTaskLog' smart constructor.
data ReplicationInstanceTaskLog = ReplicationInstanceTaskLog'
  { -- | The size, in bytes, of the replication task log.
    replicationInstanceTaskLogSize :: Core.Maybe Core.Integer,
    -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskArn :: Core.Maybe Types.String,
    -- | The name of the replication task.
    replicationTaskName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicationInstanceTaskLog' value with any optional fields omitted.
mkReplicationInstanceTaskLog ::
  ReplicationInstanceTaskLog
mkReplicationInstanceTaskLog =
  ReplicationInstanceTaskLog'
    { replicationInstanceTaskLogSize =
        Core.Nothing,
      replicationTaskArn = Core.Nothing,
      replicationTaskName = Core.Nothing
    }

-- | The size, in bytes, of the replication task log.
--
-- /Note:/ Consider using 'replicationInstanceTaskLogSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ritlReplicationInstanceTaskLogSize :: Lens.Lens' ReplicationInstanceTaskLog (Core.Maybe Core.Integer)
ritlReplicationInstanceTaskLogSize = Lens.field @"replicationInstanceTaskLogSize"
{-# DEPRECATED ritlReplicationInstanceTaskLogSize "Use generic-lens or generic-optics with 'replicationInstanceTaskLogSize' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication task.
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ritlReplicationTaskArn :: Lens.Lens' ReplicationInstanceTaskLog (Core.Maybe Types.String)
ritlReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# DEPRECATED ritlReplicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead." #-}

-- | The name of the replication task.
--
-- /Note:/ Consider using 'replicationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ritlReplicationTaskName :: Lens.Lens' ReplicationInstanceTaskLog (Core.Maybe Types.String)
ritlReplicationTaskName = Lens.field @"replicationTaskName"
{-# DEPRECATED ritlReplicationTaskName "Use generic-lens or generic-optics with 'replicationTaskName' instead." #-}

instance Core.FromJSON ReplicationInstanceTaskLog where
  parseJSON =
    Core.withObject "ReplicationInstanceTaskLog" Core.$
      \x ->
        ReplicationInstanceTaskLog'
          Core.<$> (x Core..:? "ReplicationInstanceTaskLogSize")
          Core.<*> (x Core..:? "ReplicationTaskArn")
          Core.<*> (x Core..:? "ReplicationTaskName")
