{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TableAutoScalingDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.TableAutoScalingDescription
  ( TableAutoScalingDescription (..)
  -- * Smart constructor
  , mkTableAutoScalingDescription
  -- * Lenses
  , tasdReplicas
  , tasdTableName
  , tasdTableStatus
  ) where

import qualified Network.AWS.DynamoDB.Types.ReplicaAutoScalingDescription as Types
import qualified Network.AWS.DynamoDB.Types.TableName as Types
import qualified Network.AWS.DynamoDB.Types.TableStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the auto scaling configuration for a global table.
--
-- /See:/ 'mkTableAutoScalingDescription' smart constructor.
data TableAutoScalingDescription = TableAutoScalingDescription'
  { replicas :: Core.Maybe [Types.ReplicaAutoScalingDescription]
    -- ^ Represents replicas of the global table.
  , tableName :: Core.Maybe Types.TableName
    -- ^ The name of the table.
  , tableStatus :: Core.Maybe Types.TableStatus
    -- ^ The current state of the table:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TableAutoScalingDescription' value with any optional fields omitted.
mkTableAutoScalingDescription
    :: TableAutoScalingDescription
mkTableAutoScalingDescription
  = TableAutoScalingDescription'{replicas = Core.Nothing,
                                 tableName = Core.Nothing, tableStatus = Core.Nothing}

-- | Represents replicas of the global table.
--
-- /Note:/ Consider using 'replicas' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tasdReplicas :: Lens.Lens' TableAutoScalingDescription (Core.Maybe [Types.ReplicaAutoScalingDescription])
tasdReplicas = Lens.field @"replicas"
{-# INLINEABLE tasdReplicas #-}
{-# DEPRECATED replicas "Use generic-lens or generic-optics with 'replicas' instead"  #-}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tasdTableName :: Lens.Lens' TableAutoScalingDescription (Core.Maybe Types.TableName)
tasdTableName = Lens.field @"tableName"
{-# INLINEABLE tasdTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

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
tasdTableStatus :: Lens.Lens' TableAutoScalingDescription (Core.Maybe Types.TableStatus)
tasdTableStatus = Lens.field @"tableStatus"
{-# INLINEABLE tasdTableStatus #-}
{-# DEPRECATED tableStatus "Use generic-lens or generic-optics with 'tableStatus' instead"  #-}

instance Core.FromJSON TableAutoScalingDescription where
        parseJSON
          = Core.withObject "TableAutoScalingDescription" Core.$
              \ x ->
                TableAutoScalingDescription' Core.<$>
                  (x Core..:? "Replicas") Core.<*> x Core..:? "TableName" Core.<*>
                    x Core..:? "TableStatus"
