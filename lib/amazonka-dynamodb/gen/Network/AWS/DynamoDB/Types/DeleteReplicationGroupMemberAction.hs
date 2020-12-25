{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.DeleteReplicationGroupMemberAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.DeleteReplicationGroupMemberAction
  ( DeleteReplicationGroupMemberAction (..),

    -- * Smart constructor
    mkDeleteReplicationGroupMemberAction,

    -- * Lenses
    drgmaRegionName,
  )
where

import qualified Network.AWS.DynamoDB.Types.RegionName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a replica to be deleted.
--
-- /See:/ 'mkDeleteReplicationGroupMemberAction' smart constructor.
newtype DeleteReplicationGroupMemberAction = DeleteReplicationGroupMemberAction'
  { -- | The Region where the replica exists.
    regionName :: Types.RegionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReplicationGroupMemberAction' value with any optional fields omitted.
mkDeleteReplicationGroupMemberAction ::
  -- | 'regionName'
  Types.RegionName ->
  DeleteReplicationGroupMemberAction
mkDeleteReplicationGroupMemberAction regionName =
  DeleteReplicationGroupMemberAction' {regionName}

-- | The Region where the replica exists.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drgmaRegionName :: Lens.Lens' DeleteReplicationGroupMemberAction Types.RegionName
drgmaRegionName = Lens.field @"regionName"
{-# DEPRECATED drgmaRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

instance Core.FromJSON DeleteReplicationGroupMemberAction where
  toJSON DeleteReplicationGroupMemberAction {..} =
    Core.object
      (Core.catMaybes [Core.Just ("RegionName" Core..= regionName)])
