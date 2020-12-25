{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.RemoveAllResourcePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes all the permissions from the specified resource.
module Network.AWS.WorkDocs.RemoveAllResourcePermissions
  ( -- * Creating a request
    RemoveAllResourcePermissions (..),
    mkRemoveAllResourcePermissions,

    -- ** Request lenses
    rarpResourceId,
    rarpAuthenticationToken,

    -- * Destructuring the response
    RemoveAllResourcePermissionsResponse (..),
    mkRemoveAllResourcePermissionsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkRemoveAllResourcePermissions' smart constructor.
data RemoveAllResourcePermissions = RemoveAllResourcePermissions'
  { -- | The ID of the resource.
    resourceId :: Types.ResourceId,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveAllResourcePermissions' value with any optional fields omitted.
mkRemoveAllResourcePermissions ::
  -- | 'resourceId'
  Types.ResourceId ->
  RemoveAllResourcePermissions
mkRemoveAllResourcePermissions resourceId =
  RemoveAllResourcePermissions'
    { resourceId,
      authenticationToken = Core.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarpResourceId :: Lens.Lens' RemoveAllResourcePermissions Types.ResourceId
rarpResourceId = Lens.field @"resourceId"
{-# DEPRECATED rarpResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarpAuthenticationToken :: Lens.Lens' RemoveAllResourcePermissions (Core.Maybe Types.AuthenticationToken)
rarpAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED rarpAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

instance Core.AWSRequest RemoveAllResourcePermissions where
  type
    Rs RemoveAllResourcePermissions =
      RemoveAllResourcePermissionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/api/v1/resources/" Core.<> (Core.toText resourceId)
                Core.<> ("/permissions")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response =
    Response.receiveNull RemoveAllResourcePermissionsResponse'

-- | /See:/ 'mkRemoveAllResourcePermissionsResponse' smart constructor.
data RemoveAllResourcePermissionsResponse = RemoveAllResourcePermissionsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveAllResourcePermissionsResponse' value with any optional fields omitted.
mkRemoveAllResourcePermissionsResponse ::
  RemoveAllResourcePermissionsResponse
mkRemoveAllResourcePermissionsResponse =
  RemoveAllResourcePermissionsResponse'
