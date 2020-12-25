{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DeleteAccessControlRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an access control rule for the specified WorkMail organization.
module Network.AWS.WorkMail.DeleteAccessControlRule
  ( -- * Creating a request
    DeleteAccessControlRule (..),
    mkDeleteAccessControlRule,

    -- ** Request lenses
    dacrOrganizationId,
    dacrName,

    -- * Destructuring the response
    DeleteAccessControlRuleResponse (..),
    mkDeleteAccessControlRuleResponse,

    -- ** Response lenses
    dacrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDeleteAccessControlRule' smart constructor.
data DeleteAccessControlRule = DeleteAccessControlRule'
  { -- | The identifier for the organization.
    organizationId :: Types.OrganizationId,
    -- | The name of the access control rule.
    name :: Types.AccessControlRuleName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAccessControlRule' value with any optional fields omitted.
mkDeleteAccessControlRule ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'name'
  Types.AccessControlRuleName ->
  DeleteAccessControlRule
mkDeleteAccessControlRule organizationId name =
  DeleteAccessControlRule' {organizationId, name}

-- | The identifier for the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacrOrganizationId :: Lens.Lens' DeleteAccessControlRule Types.OrganizationId
dacrOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED dacrOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The name of the access control rule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacrName :: Lens.Lens' DeleteAccessControlRule Types.AccessControlRuleName
dacrName = Lens.field @"name"
{-# DEPRECATED dacrName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON DeleteAccessControlRule where
  toJSON DeleteAccessControlRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.AWSRequest DeleteAccessControlRule where
  type Rs DeleteAccessControlRule = DeleteAccessControlRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "WorkMailService.DeleteAccessControlRule")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAccessControlRuleResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteAccessControlRuleResponse' smart constructor.
newtype DeleteAccessControlRuleResponse = DeleteAccessControlRuleResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteAccessControlRuleResponse' value with any optional fields omitted.
mkDeleteAccessControlRuleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteAccessControlRuleResponse
mkDeleteAccessControlRuleResponse responseStatus =
  DeleteAccessControlRuleResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacrrrsResponseStatus :: Lens.Lens' DeleteAccessControlRuleResponse Core.Int
dacrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dacrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
