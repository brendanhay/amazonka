{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TableAutoScalingDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TableAutoScalingDescription
  ( TableAutoScalingDescription (..),

    -- * Smart constructor
    mkTableAutoScalingDescription,

    -- * Lenses
    tasdTableStatus,
    tasdReplicas,
    tasdTableName,
  )
where

import Network.AWS.DynamoDB.Types.ReplicaAutoScalingDescription
import Network.AWS.DynamoDB.Types.TableStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the auto scaling configuration for a global table.
--
-- /See:/ 'mkTableAutoScalingDescription' smart constructor.
data TableAutoScalingDescription = TableAutoScalingDescription'
  { tableStatus ::
      Lude.Maybe TableStatus,
    replicas ::
      Lude.Maybe
        [ReplicaAutoScalingDescription],
    tableName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TableAutoScalingDescription' with the minimum fields required to make a request.
--
-- * 'replicas' - Represents replicas of the global table.
-- * 'tableName' - The name of the table.
-- * 'tableStatus' - The current state of the table:
--
--
--     * @CREATING@ - The table is being created.
--
--
--     * @UPDATING@ - The table is being updated.
--
--
--     * @DELETING@ - The table is being deleted.
--
--
--     * @ACTIVE@ - The table is ready for use.
mkTableAutoScalingDescription ::
  TableAutoScalingDescription
mkTableAutoScalingDescription =
  TableAutoScalingDescription'
    { tableStatus = Lude.Nothing,
      replicas = Lude.Nothing,
      tableName = Lude.Nothing
    }

-- | The current state of the table:
--
--
--     * @CREATING@ - The table is being created.
--
--
--     * @UPDATING@ - The table is being updated.
--
--
--     * @DELETING@ - The table is being deleted.
--
--
--     * @ACTIVE@ - The table is ready for use.
--
--
--
-- /Note:/ Consider using 'tableStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tasdTableStatus :: Lens.Lens' TableAutoScalingDescription (Lude.Maybe TableStatus)
tasdTableStatus = Lens.lens (tableStatus :: TableAutoScalingDescription -> Lude.Maybe TableStatus) (\s a -> s {tableStatus = a} :: TableAutoScalingDescription)
{-# DEPRECATED tasdTableStatus "Use generic-lens or generic-optics with 'tableStatus' instead." #-}

-- | Represents replicas of the global table.
--
-- /Note:/ Consider using 'replicas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tasdReplicas :: Lens.Lens' TableAutoScalingDescription (Lude.Maybe [ReplicaAutoScalingDescription])
tasdReplicas = Lens.lens (replicas :: TableAutoScalingDescription -> Lude.Maybe [ReplicaAutoScalingDescription]) (\s a -> s {replicas = a} :: TableAutoScalingDescription)
{-# DEPRECATED tasdReplicas "Use generic-lens or generic-optics with 'replicas' instead." #-}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tasdTableName :: Lens.Lens' TableAutoScalingDescription (Lude.Maybe Lude.Text)
tasdTableName = Lens.lens (tableName :: TableAutoScalingDescription -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: TableAutoScalingDescription)
{-# DEPRECATED tasdTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.FromJSON TableAutoScalingDescription where
  parseJSON =
    Lude.withObject
      "TableAutoScalingDescription"
      ( \x ->
          TableAutoScalingDescription'
            Lude.<$> (x Lude..:? "TableStatus")
            Lude.<*> (x Lude..:? "Replicas" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TableName")
      )
