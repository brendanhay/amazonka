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
    ritlReplicationTaskName,
    ritlReplicationTaskARN,
    ritlReplicationInstanceTaskLogSize,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains metadata for a replication instance task log.
--
-- /See:/ 'mkReplicationInstanceTaskLog' smart constructor.
data ReplicationInstanceTaskLog = ReplicationInstanceTaskLog'
  { replicationTaskName ::
      Lude.Maybe Lude.Text,
    replicationTaskARN ::
      Lude.Maybe Lude.Text,
    replicationInstanceTaskLogSize ::
      Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationInstanceTaskLog' with the minimum fields required to make a request.
--
-- * 'replicationInstanceTaskLogSize' - The size, in bytes, of the replication task log.
-- * 'replicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
-- * 'replicationTaskName' - The name of the replication task.
mkReplicationInstanceTaskLog ::
  ReplicationInstanceTaskLog
mkReplicationInstanceTaskLog =
  ReplicationInstanceTaskLog'
    { replicationTaskName = Lude.Nothing,
      replicationTaskARN = Lude.Nothing,
      replicationInstanceTaskLogSize = Lude.Nothing
    }

-- | The name of the replication task.
--
-- /Note:/ Consider using 'replicationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ritlReplicationTaskName :: Lens.Lens' ReplicationInstanceTaskLog (Lude.Maybe Lude.Text)
ritlReplicationTaskName = Lens.lens (replicationTaskName :: ReplicationInstanceTaskLog -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskName = a} :: ReplicationInstanceTaskLog)
{-# DEPRECATED ritlReplicationTaskName "Use generic-lens or generic-optics with 'replicationTaskName' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication task.
--
-- /Note:/ Consider using 'replicationTaskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ritlReplicationTaskARN :: Lens.Lens' ReplicationInstanceTaskLog (Lude.Maybe Lude.Text)
ritlReplicationTaskARN = Lens.lens (replicationTaskARN :: ReplicationInstanceTaskLog -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskARN = a} :: ReplicationInstanceTaskLog)
{-# DEPRECATED ritlReplicationTaskARN "Use generic-lens or generic-optics with 'replicationTaskARN' instead." #-}

-- | The size, in bytes, of the replication task log.
--
-- /Note:/ Consider using 'replicationInstanceTaskLogSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ritlReplicationInstanceTaskLogSize :: Lens.Lens' ReplicationInstanceTaskLog (Lude.Maybe Lude.Integer)
ritlReplicationInstanceTaskLogSize = Lens.lens (replicationInstanceTaskLogSize :: ReplicationInstanceTaskLog -> Lude.Maybe Lude.Integer) (\s a -> s {replicationInstanceTaskLogSize = a} :: ReplicationInstanceTaskLog)
{-# DEPRECATED ritlReplicationInstanceTaskLogSize "Use generic-lens or generic-optics with 'replicationInstanceTaskLogSize' instead." #-}

instance Lude.FromJSON ReplicationInstanceTaskLog where
  parseJSON =
    Lude.withObject
      "ReplicationInstanceTaskLog"
      ( \x ->
          ReplicationInstanceTaskLog'
            Lude.<$> (x Lude..:? "ReplicationTaskName")
            Lude.<*> (x Lude..:? "ReplicationTaskArn")
            Lude.<*> (x Lude..:? "ReplicationInstanceTaskLogSize")
      )
