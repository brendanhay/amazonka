{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalTable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.GlobalTable
  ( GlobalTable (..)
  -- * Smart constructor
  , mkGlobalTable
  -- * Lenses
  , gtGlobalTableName
  , gtReplicationGroup
  ) where

import qualified Network.AWS.DynamoDB.Types.GlobalTableName as Types
import qualified Network.AWS.DynamoDB.Types.Replica as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the properties of a global table.
--
-- /See:/ 'mkGlobalTable' smart constructor.
data GlobalTable = GlobalTable'
  { globalTableName :: Core.Maybe Types.GlobalTableName
    -- ^ The global table name.
  , replicationGroup :: Core.Maybe [Types.Replica]
    -- ^ The Regions where the global table has replicas.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GlobalTable' value with any optional fields omitted.
mkGlobalTable
    :: GlobalTable
mkGlobalTable
  = GlobalTable'{globalTableName = Core.Nothing,
                 replicationGroup = Core.Nothing}

-- | The global table name.
--
-- /Note:/ Consider using 'globalTableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtGlobalTableName :: Lens.Lens' GlobalTable (Core.Maybe Types.GlobalTableName)
gtGlobalTableName = Lens.field @"globalTableName"
{-# INLINEABLE gtGlobalTableName #-}
{-# DEPRECATED globalTableName "Use generic-lens or generic-optics with 'globalTableName' instead"  #-}

-- | The Regions where the global table has replicas.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtReplicationGroup :: Lens.Lens' GlobalTable (Core.Maybe [Types.Replica])
gtReplicationGroup = Lens.field @"replicationGroup"
{-# INLINEABLE gtReplicationGroup #-}
{-# DEPRECATED replicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead"  #-}

instance Core.FromJSON GlobalTable where
        parseJSON
          = Core.withObject "GlobalTable" Core.$
              \ x ->
                GlobalTable' Core.<$>
                  (x Core..:? "GlobalTableName") Core.<*>
                    x Core..:? "ReplicationGroup"
