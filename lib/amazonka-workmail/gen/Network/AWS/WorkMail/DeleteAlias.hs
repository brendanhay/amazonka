{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DeleteAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove one or more specified aliases from a set of aliases for a given user.
module Network.AWS.WorkMail.DeleteAlias
  ( -- * Creating a request
    DeleteAlias (..),
    mkDeleteAlias,

    -- ** Request lenses
    daOrganizationId,
    daEntityId,
    daAlias,

    -- * Destructuring the response
    DeleteAliasResponse (..),
    mkDeleteAliasResponse,

    -- ** Response lenses
    darrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDeleteAlias' smart constructor.
data DeleteAlias = DeleteAlias'
  { -- | The identifier for the organization under which the user exists.
    organizationId :: Types.OrganizationId,
    -- | The identifier for the member (user or group) from which to have the aliases removed.
    entityId :: Types.WorkMailIdentifier,
    -- | The aliases to be removed from the user's set of aliases. Duplicate entries in the list are collapsed into single entries (the list is transformed into a set).
    alias :: Types.Alias
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAlias' value with any optional fields omitted.
mkDeleteAlias ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'entityId'
  Types.WorkMailIdentifier ->
  -- | 'alias'
  Types.Alias ->
  DeleteAlias
mkDeleteAlias organizationId entityId alias =
  DeleteAlias' {organizationId, entityId, alias}

-- | The identifier for the organization under which the user exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daOrganizationId :: Lens.Lens' DeleteAlias Types.OrganizationId
daOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED daOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier for the member (user or group) from which to have the aliases removed.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daEntityId :: Lens.Lens' DeleteAlias Types.WorkMailIdentifier
daEntityId = Lens.field @"entityId"
{-# DEPRECATED daEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The aliases to be removed from the user's set of aliases. Duplicate entries in the list are collapsed into single entries (the list is transformed into a set).
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAlias :: Lens.Lens' DeleteAlias Types.Alias
daAlias = Lens.field @"alias"
{-# DEPRECATED daAlias "Use generic-lens or generic-optics with 'alias' instead." #-}

instance Core.FromJSON DeleteAlias where
  toJSON DeleteAlias {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("EntityId" Core..= entityId),
            Core.Just ("Alias" Core..= alias)
          ]
      )

instance Core.AWSRequest DeleteAlias where
  type Rs DeleteAlias = DeleteAliasResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.DeleteAlias")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAliasResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteAliasResponse' smart constructor.
newtype DeleteAliasResponse = DeleteAliasResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAliasResponse' value with any optional fields omitted.
mkDeleteAliasResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteAliasResponse
mkDeleteAliasResponse responseStatus =
  DeleteAliasResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darrsResponseStatus :: Lens.Lens' DeleteAliasResponse Core.Int
darrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED darrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
