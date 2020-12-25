{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ListMailboxPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the mailbox permissions associated with a user, group, or resource mailbox.
--
-- This operation returns paginated results.
module Network.AWS.WorkMail.ListMailboxPermissions
  ( -- * Creating a request
    ListMailboxPermissions (..),
    mkListMailboxPermissions,

    -- ** Request lenses
    lmpOrganizationId,
    lmpEntityId,
    lmpMaxResults,
    lmpNextToken,

    -- * Destructuring the response
    ListMailboxPermissionsResponse (..),
    mkListMailboxPermissionsResponse,

    -- ** Response lenses
    lmprrsNextToken,
    lmprrsPermissions,
    lmprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkListMailboxPermissions' smart constructor.
data ListMailboxPermissions = ListMailboxPermissions'
  { -- | The identifier of the organization under which the user, group, or resource exists.
    organizationId :: Types.OrganizationId,
    -- | The identifier of the user, group, or resource for which to list mailbox permissions.
    entityId :: Types.EntityId,
    -- | The maximum number of results to return in a single call.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMailboxPermissions' value with any optional fields omitted.
mkListMailboxPermissions ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'entityId'
  Types.EntityId ->
  ListMailboxPermissions
mkListMailboxPermissions organizationId entityId =
  ListMailboxPermissions'
    { organizationId,
      entityId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the organization under which the user, group, or resource exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpOrganizationId :: Lens.Lens' ListMailboxPermissions Types.OrganizationId
lmpOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED lmpOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier of the user, group, or resource for which to list mailbox permissions.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpEntityId :: Lens.Lens' ListMailboxPermissions Types.EntityId
lmpEntityId = Lens.field @"entityId"
{-# DEPRECATED lmpEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The maximum number of results to return in a single call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpMaxResults :: Lens.Lens' ListMailboxPermissions (Core.Maybe Core.Natural)
lmpMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lmpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to use to retrieve the next page of results. The first call does not contain any tokens.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmpNextToken :: Lens.Lens' ListMailboxPermissions (Core.Maybe Types.NextToken)
lmpNextToken = Lens.field @"nextToken"
{-# DEPRECATED lmpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListMailboxPermissions where
  toJSON ListMailboxPermissions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("EntityId" Core..= entityId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListMailboxPermissions where
  type Rs ListMailboxPermissions = ListMailboxPermissionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "WorkMailService.ListMailboxPermissions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMailboxPermissionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Permissions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListMailboxPermissions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"permissions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListMailboxPermissionsResponse' smart constructor.
data ListMailboxPermissionsResponse = ListMailboxPermissionsResponse'
  { -- | The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
    nextToken :: Core.Maybe Types.NextToken,
    -- | One page of the user, group, or resource mailbox permissions.
    permissions :: Core.Maybe [Types.Permission],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMailboxPermissionsResponse' value with any optional fields omitted.
mkListMailboxPermissionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListMailboxPermissionsResponse
mkListMailboxPermissionsResponse responseStatus =
  ListMailboxPermissionsResponse'
    { nextToken = Core.Nothing,
      permissions = Core.Nothing,
      responseStatus
    }

-- | The token to use to retrieve the next page of results. The value is "null" when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprrsNextToken :: Lens.Lens' ListMailboxPermissionsResponse (Core.Maybe Types.NextToken)
lmprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lmprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | One page of the user, group, or resource mailbox permissions.
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprrsPermissions :: Lens.Lens' ListMailboxPermissionsResponse (Core.Maybe [Types.Permission])
lmprrsPermissions = Lens.field @"permissions"
{-# DEPRECATED lmprrsPermissions "Use generic-lens or generic-optics with 'permissions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmprrsResponseStatus :: Lens.Lens' ListMailboxPermissionsResponse Core.Int
lmprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lmprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
