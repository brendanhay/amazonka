{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalTableDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.GlobalTableDescription
  ( GlobalTableDescription (..)
  -- * Smart constructor
  , mkGlobalTableDescription
  -- * Lenses
  , gtdCreationDateTime
  , gtdGlobalTableArn
  , gtdGlobalTableName
  , gtdGlobalTableStatus
  , gtdReplicationGroup
  ) where

import qualified Network.AWS.DynamoDB.Types.GlobalTableArnString as Types
import qualified Network.AWS.DynamoDB.Types.GlobalTableName as Types
import qualified Network.AWS.DynamoDB.Types.GlobalTableStatus as Types
import qualified Network.AWS.DynamoDB.Types.ReplicaDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains details about the global table.
--
-- /See:/ 'mkGlobalTableDescription' smart constructor.
data GlobalTableDescription = GlobalTableDescription'
  { creationDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The creation time of the global table.
  , globalTableArn :: Core.Maybe Types.GlobalTableArnString
    -- ^ The unique identifier of the global table.
  , globalTableName :: Core.Maybe Types.GlobalTableName
    -- ^ The global table name.
  , globalTableStatus :: Core.Maybe Types.GlobalTableStatus
    -- ^ The current state of the global table:
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
  , replicationGroup :: Core.Maybe [Types.ReplicaDescription]
    -- ^ The Regions where the global table has replicas.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GlobalTableDescription' value with any optional fields omitted.
mkGlobalTableDescription
    :: GlobalTableDescription
mkGlobalTableDescription
  = GlobalTableDescription'{creationDateTime = Core.Nothing,
                            globalTableArn = Core.Nothing, globalTableName = Core.Nothing,
                            globalTableStatus = Core.Nothing, replicationGroup = Core.Nothing}

-- | The creation time of the global table.
--
-- /Note:/ Consider using 'creationDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdCreationDateTime :: Lens.Lens' GlobalTableDescription (Core.Maybe Core.NominalDiffTime)
gtdCreationDateTime = Lens.field @"creationDateTime"
{-# INLINEABLE gtdCreationDateTime #-}
{-# DEPRECATED creationDateTime "Use generic-lens or generic-optics with 'creationDateTime' instead"  #-}

-- | The unique identifier of the global table.
--
-- /Note:/ Consider using 'globalTableArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdGlobalTableArn :: Lens.Lens' GlobalTableDescription (Core.Maybe Types.GlobalTableArnString)
gtdGlobalTableArn = Lens.field @"globalTableArn"
{-# INLINEABLE gtdGlobalTableArn #-}
{-# DEPRECATED globalTableArn "Use generic-lens or generic-optics with 'globalTableArn' instead"  #-}

-- | The global table name.
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdGlobalTableName :: Lens.Lens' GlobalTableDescription (Core.Maybe Types.GlobalTableName)
gtdGlobalTableName = Lens.field @"globalTableName"
{-# INLINEABLE gtdGlobalTableName #-}
{-# DEPRECATED globalTableName "Use generic-lens or generic-optics with 'globalTableName' instead"  #-}

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
gtdGlobalTableStatus :: Lens.Lens' GlobalTableDescription (Core.Maybe Types.GlobalTableStatus)
gtdGlobalTableStatus = Lens.field @"globalTableStatus"
{-# INLINEABLE gtdGlobalTableStatus #-}
{-# DEPRECATED globalTableStatus "Use generic-lens or generic-optics with 'globalTableStatus' instead"  #-}

-- | The Regions where the global table has replicas.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtdReplicationGroup :: Lens.Lens' GlobalTableDescription (Core.Maybe [Types.ReplicaDescription])
gtdReplicationGroup = Lens.field @"replicationGroup"
{-# INLINEABLE gtdReplicationGroup #-}
{-# DEPRECATED replicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead"  #-}

instance Core.FromJSON GlobalTableDescription where
        parseJSON
          = Core.withObject "GlobalTableDescription" Core.$
              \ x ->
                GlobalTableDescription' Core.<$>
                  (x Core..:? "CreationDateTime") Core.<*>
                    x Core..:? "GlobalTableArn"
                    Core.<*> x Core..:? "GlobalTableName"
                    Core.<*> x Core..:? "GlobalTableStatus"
                    Core.<*> x Core..:? "ReplicationGroup"
