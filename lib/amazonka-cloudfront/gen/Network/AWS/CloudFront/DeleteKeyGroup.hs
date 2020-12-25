{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteKeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a key group.
--
-- You cannot delete a key group that is referenced in a cache behavior. First update your distributions to remove the key group from all cache behaviors, then delete the key group.
-- To delete a key group, you must provide the key group’s identifier and version. To get these values, use @ListKeyGroups@ followed by @GetKeyGroup@ or @GetKeyGroupConfig@ .
module Network.AWS.CloudFront.DeleteKeyGroup
  ( -- * Creating a request
    DeleteKeyGroup (..),
    mkDeleteKeyGroup,

    -- ** Request lenses
    dkgId,
    dkgIfMatch,

    -- * Destructuring the response
    DeleteKeyGroupResponse (..),
    mkDeleteKeyGroupResponse,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteKeyGroup' smart constructor.
data DeleteKeyGroup = DeleteKeyGroup'
  { -- | The identifier of the key group that you are deleting. To get the identifier, use @ListKeyGroups@ .
    id :: Types.String,
    -- | The version of the key group that you are deleting. The version is the key group’s @ETag@ value. To get the @ETag@ , use @GetKeyGroup@ or @GetKeyGroupConfig@ .
    ifMatch :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteKeyGroup' value with any optional fields omitted.
mkDeleteKeyGroup ::
  -- | 'id'
  Types.String ->
  DeleteKeyGroup
mkDeleteKeyGroup id = DeleteKeyGroup' {id, ifMatch = Core.Nothing}

-- | The identifier of the key group that you are deleting. To get the identifier, use @ListKeyGroups@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkgId :: Lens.Lens' DeleteKeyGroup Types.String
dkgId = Lens.field @"id"
{-# DEPRECATED dkgId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The version of the key group that you are deleting. The version is the key group’s @ETag@ value. To get the @ETag@ , use @GetKeyGroup@ or @GetKeyGroupConfig@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkgIfMatch :: Lens.Lens' DeleteKeyGroup (Core.Maybe Types.String)
dkgIfMatch = Lens.field @"ifMatch"
{-# DEPRECATED dkgIfMatch "Use generic-lens or generic-optics with 'ifMatch' instead." #-}

instance Core.AWSRequest DeleteKeyGroup where
  type Rs DeleteKeyGroup = DeleteKeyGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath ("/2020-05-31/key-group/" Core.<> (Core.toText id)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.toHeaders "If-Match" ifMatch,
        Core._rqBody = ""
      }
  response = Response.receiveNull DeleteKeyGroupResponse'

-- | /See:/ 'mkDeleteKeyGroupResponse' smart constructor.
data DeleteKeyGroupResponse = DeleteKeyGroupResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteKeyGroupResponse' value with any optional fields omitted.
mkDeleteKeyGroupResponse ::
  DeleteKeyGroupResponse
mkDeleteKeyGroupResponse = DeleteKeyGroupResponse'
