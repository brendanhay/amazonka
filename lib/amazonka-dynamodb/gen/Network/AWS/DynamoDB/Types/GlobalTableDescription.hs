{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalTableDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.GlobalTableDescription
  ( GlobalTableDescription (..),

    -- * Smart constructor
    mkGlobalTableDescription,

    -- * Lenses
    gtdGlobalTableStatus,
    gtdGlobalTableName,
    gtdGlobalTableARN,
    gtdCreationDateTime,
    gtdReplicationGroup,
  )
where

import Network.AWS.DynamoDB.Types.GlobalTableStatus
import Network.AWS.DynamoDB.Types.ReplicaDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about the global table.
--
-- /See:/ 'mkGlobalTableDescription' smart constructor.
data GlobalTableDescription = GlobalTableDescription'
  { -- | The current state of the global table:
    --
    --
    --     * @CREATING@ - The global table is being created.
    --
    --
    --     * @UPDATING@ - The global table is being updated.
    --
    --
    --     * @DELETING@ - The global table is being deleted.
    --
    --
    --     * @ACTIVE@ - The global table is ready for use.
    globalTableStatus :: Lude.Maybe GlobalTableStatus,
    -- | The global table name.
    globalTableName :: Lude.Maybe Lude.Text,
    -- | The unique identifier of the global table.
    globalTableARN :: Lude.Maybe Lude.Text,
    -- | The creation time of the global table.
    creationDateTime :: Lude.Maybe Lude.Timestamp,
    -- | The Regions where the global table has replicas.
    replicationGroup :: Lude.Maybe [ReplicaDescription]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GlobalTableDescription' with the minimum fields required to make a request.
--
-- * 'globalTableStatus' - The current state of the global table:
--
--
--     * @CREATING@ - The global table is being created.
--
--
--     * @UPDATING@ - The global table is being updated.
--
--
--     * @DELETING@ - The global table is being deleted.
--
--
--     * @ACTIVE@ - The global table is ready for use.
--
--
-- * 'globalTableName' - The global table name.
-- * 'globalTableARN' - The unique identifier of the global table.
-- * 'creationDateTime' - The creation time of the global table.
-- * 'replicationGroup' - The Regions where the global table has replicas.
mkGlobalTableDescription ::
  GlobalTableDescription
mkGlobalTableDescription =
  GlobalTableDescription'
    { globalTableStatus = Lude.Nothing,
      globalTableName = Lude.Nothing,
      globalTableARN = Lude.Nothing,
      creationDateTime = Lude.Nothing,
      replicationGroup = Lude.Nothing
    }

-- | The current state of the global table:
--
--
--     * @CREATING@ - The global table is being created.
--
--
--     * @UPDATING@ - The global table is being updated.
--
--
--     * @DELETING@ - The global table is being deleted.
--
--
--     * @ACTIVE@ - The global table is ready for use.
--
--
--
-- /Note:/ Consider using 'globalTableStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdGlobalTableStatus :: Lens.Lens' GlobalTableDescription (Lude.Maybe GlobalTableStatus)
gtdGlobalTableStatus = Lens.lens (globalTableStatus :: GlobalTableDescription -> Lude.Maybe GlobalTableStatus) (\s a -> s {globalTableStatus = a} :: GlobalTableDescription)
{-# DEPRECATED gtdGlobalTableStatus "Use generic-lens or generic-optics with 'globalTableStatus' instead." #-}

-- | The global table name.
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdGlobalTableName :: Lens.Lens' GlobalTableDescription (Lude.Maybe Lude.Text)
gtdGlobalTableName = Lens.lens (globalTableName :: GlobalTableDescription -> Lude.Maybe Lude.Text) (\s a -> s {globalTableName = a} :: GlobalTableDescription)
{-# DEPRECATED gtdGlobalTableName "Use generic-lens or generic-optics with 'globalTableName' instead." #-}

-- | The unique identifier of the global table.
--
-- /Note:/ Consider using 'globalTableARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdGlobalTableARN :: Lens.Lens' GlobalTableDescription (Lude.Maybe Lude.Text)
gtdGlobalTableARN = Lens.lens (globalTableARN :: GlobalTableDescription -> Lude.Maybe Lude.Text) (\s a -> s {globalTableARN = a} :: GlobalTableDescription)
{-# DEPRECATED gtdGlobalTableARN "Use generic-lens or generic-optics with 'globalTableARN' instead." #-}

-- | The creation time of the global table.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdCreationDateTime :: Lens.Lens' GlobalTableDescription (Lude.Maybe Lude.Timestamp)
gtdCreationDateTime = Lens.lens (creationDateTime :: GlobalTableDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDateTime = a} :: GlobalTableDescription)
{-# DEPRECATED gtdCreationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead." #-}

-- | The Regions where the global table has replicas.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdReplicationGroup :: Lens.Lens' GlobalTableDescription (Lude.Maybe [ReplicaDescription])
gtdReplicationGroup = Lens.lens (replicationGroup :: GlobalTableDescription -> Lude.Maybe [ReplicaDescription]) (\s a -> s {replicationGroup = a} :: GlobalTableDescription)
{-# DEPRECATED gtdReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

instance Lude.FromJSON GlobalTableDescription where
  parseJSON =
    Lude.withObject
      "GlobalTableDescription"
      ( \x ->
          GlobalTableDescription'
            Lude.<$> (x Lude..:? "GlobalTableStatus")
            Lude.<*> (x Lude..:? "GlobalTableName")
            Lude.<*> (x Lude..:? "GlobalTableArn")
            Lude.<*> (x Lude..:? "CreationDateTime")
            Lude.<*> (x Lude..:? "ReplicationGroup" Lude..!= Lude.mempty)
      )
