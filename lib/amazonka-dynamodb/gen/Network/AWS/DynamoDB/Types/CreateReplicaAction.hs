{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.CreateReplicaAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.CreateReplicaAction
  ( CreateReplicaAction (..)
  -- * Smart constructor
  , mkCreateReplicaAction
  -- * Lenses
  , craRegionName
  ) where

import qualified Network.AWS.DynamoDB.Types.RegionName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a replica to be added.
--
-- /See:/ 'mkCreateReplicaAction' smart constructor.
newtype CreateReplicaAction = CreateReplicaAction'
  { regionName :: Types.RegionName
    -- ^ The Region of the replica to be added.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReplicaAction' value with any optional fields omitted.
mkCreateReplicaAction
    :: Types.RegionName -- ^ 'regionName'
    -> CreateReplicaAction
mkCreateReplicaAction regionName = CreateReplicaAction'{regionName}

-- | The Region of the replica to be added.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
craRegionName :: Lens.Lens' CreateReplicaAction Types.RegionName
craRegionName = Lens.field @"regionName"
{-# INLINEABLE craRegionName #-}
{-# DEPRECATED regionName "Use generic-lens or generic-optics with 'regionName' instead"  #-}

instance Core.FromJSON CreateReplicaAction where
        toJSON CreateReplicaAction{..}
          = Core.object
              (Core.catMaybes [Core.Just ("RegionName" Core..= regionName)])
