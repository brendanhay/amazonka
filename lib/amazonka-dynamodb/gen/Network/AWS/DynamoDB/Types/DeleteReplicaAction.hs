{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.DeleteReplicaAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.DeleteReplicaAction
  ( DeleteReplicaAction (..)
  -- * Smart constructor
  , mkDeleteReplicaAction
  -- * Lenses
  , draRegionName
  ) where

import qualified Network.AWS.DynamoDB.Types.RegionName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a replica to be removed.
--
-- /See:/ 'mkDeleteReplicaAction' smart constructor.
newtype DeleteReplicaAction = DeleteReplicaAction'
  { regionName :: Types.RegionName
    -- ^ The Region of the replica to be removed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReplicaAction' value with any optional fields omitted.
mkDeleteReplicaAction
    :: Types.RegionName -- ^ 'regionName'
    -> DeleteReplicaAction
mkDeleteReplicaAction regionName = DeleteReplicaAction'{regionName}

-- | The Region of the replica to be removed.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
draRegionName :: Lens.Lens' DeleteReplicaAction Types.RegionName
draRegionName = Lens.field @"regionName"
{-# INLINEABLE draRegionName #-}
{-# DEPRECATED regionName "Use generic-lens or generic-optics with 'regionName' instead"  #-}

instance Core.FromJSON DeleteReplicaAction where
        toJSON DeleteReplicaAction{..}
          = Core.object
              (Core.catMaybes [Core.Just ("RegionName" Core..= regionName)])
