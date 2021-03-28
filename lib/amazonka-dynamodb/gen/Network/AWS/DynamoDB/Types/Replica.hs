{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.Replica
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.Replica
  ( Replica (..)
  -- * Smart constructor
  , mkReplica
  -- * Lenses
  , rRegionName
  ) where

import qualified Network.AWS.DynamoDB.Types.RegionName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the properties of a replica.
--
-- /See:/ 'mkReplica' smart constructor.
newtype Replica = Replica'
  { regionName :: Core.Maybe Types.RegionName
    -- ^ The Region where the replica needs to be created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Replica' value with any optional fields omitted.
mkReplica
    :: Replica
mkReplica = Replica'{regionName = Core.Nothing}

-- | The Region where the replica needs to be created.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRegionName :: Lens.Lens' Replica (Core.Maybe Types.RegionName)
rRegionName = Lens.field @"regionName"
{-# INLINEABLE rRegionName #-}
{-# DEPRECATED regionName "Use generic-lens or generic-optics with 'regionName' instead"  #-}

instance Core.FromJSON Replica where
        toJSON Replica{..}
          = Core.object
              (Core.catMaybes [("RegionName" Core..=) Core.<$> regionName])

instance Core.FromJSON Replica where
        parseJSON
          = Core.withObject "Replica" Core.$
              \ x -> Replica' Core.<$> (x Core..:? "RegionName")
