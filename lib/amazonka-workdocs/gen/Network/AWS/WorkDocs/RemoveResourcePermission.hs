{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.RemoveResourcePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the permission for the specified principal from the specified resource.
module Network.AWS.WorkDocs.RemoveResourcePermission
  ( -- * Creating a request
    RemoveResourcePermission (..),
    mkRemoveResourcePermission,

    -- ** Request lenses
    rrpResourceId,
    rrpPrincipalId,
    rrpAuthenticationToken,
    rrpPrincipalType,

    -- * Destructuring the response
    RemoveResourcePermissionResponse (..),
    mkRemoveResourcePermissionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkRemoveResourcePermission' smart constructor.
data RemoveResourcePermission = RemoveResourcePermission'
  { -- | The ID of the resource.
    resourceId :: Types.ResourceId,
    -- | The principal ID of the resource.
    principalId :: Types.IdType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | The principal type of the resource.
    principalType :: Core.Maybe Types.PrincipalType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveResourcePermission' value with any optional fields omitted.
mkRemoveResourcePermission ::
  -- | 'resourceId'
  Types.ResourceId ->
  -- | 'principalId'
  Types.IdType ->
  RemoveResourcePermission
mkRemoveResourcePermission resourceId principalId =
  RemoveResourcePermission'
    { resourceId,
      principalId,
      authenticationToken = Core.Nothing,
      principalType = Core.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrpResourceId :: Lens.Lens' RemoveResourcePermission Types.ResourceId
rrpResourceId = Lens.field @"resourceId"
{-# DEPRECATED rrpResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The principal ID of the resource.
--
-- /Note:/ Consider using 'principalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrpPrincipalId :: Lens.Lens' RemoveResourcePermission Types.IdType
rrpPrincipalId = Lens.field @"principalId"
{-# DEPRECATED rrpPrincipalId "Use generic-lens or generic-optics with 'principalId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrpAuthenticationToken :: Lens.Lens' RemoveResourcePermission (Core.Maybe Types.AuthenticationHeaderType)
rrpAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED rrpAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The principal type of the resource.
--
-- /Note:/ Consider using 'principalType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrpPrincipalType :: Lens.Lens' RemoveResourcePermission (Core.Maybe Types.PrincipalType)
rrpPrincipalType = Lens.field @"principalType"
{-# DEPRECATED rrpPrincipalType "Use generic-lens or generic-optics with 'principalType' instead." #-}

instance Core.AWSRequest RemoveResourcePermission where
  type Rs RemoveResourcePermission = RemoveResourcePermissionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/api/v1/resources/" Core.<> (Core.toText resourceId)
                Core.<> ("/permissions/")
                Core.<> (Core.toText principalId)
            ),
        Core._rqQuery = Core.toQueryValue "type" Core.<$> principalType,
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response = Response.receiveNull RemoveResourcePermissionResponse'

-- | /See:/ 'mkRemoveResourcePermissionResponse' smart constructor.
data RemoveResourcePermissionResponse = RemoveResourcePermissionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveResourcePermissionResponse' value with any optional fields omitted.
mkRemoveResourcePermissionResponse ::
  RemoveResourcePermissionResponse
mkRemoveResourcePermissionResponse =
  RemoveResourcePermissionResponse'
