{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.PutAccessControlRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new access control rule for the specified organization. The rule allows or denies access to the organization for the specified IPv4 addresses, access protocol actions, and user IDs. Adding a new rule with the same name as an existing rule replaces the older rule.
module Network.AWS.WorkMail.PutAccessControlRule
  ( -- * Creating a request
    PutAccessControlRule (..),
    mkPutAccessControlRule,

    -- ** Request lenses
    pacrName,
    pacrEffect,
    pacrDescription,
    pacrOrganizationId,
    pacrActions,
    pacrIpRanges,
    pacrNotActions,
    pacrNotIpRanges,
    pacrNotUserIds,
    pacrUserIds,

    -- * Destructuring the response
    PutAccessControlRuleResponse (..),
    mkPutAccessControlRuleResponse,

    -- ** Response lenses
    pacrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkPutAccessControlRule' smart constructor.
data PutAccessControlRule = PutAccessControlRule'
  { -- | The rule name.
    name :: Types.AccessControlRuleName,
    -- | The rule effect.
    effect :: Types.AccessControlRuleEffect,
    -- | The rule description.
    description :: Types.AccessControlRuleDescription,
    -- | The identifier of the organization.
    organizationId :: Types.OrganizationId,
    -- | Access protocol actions to include in the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
    actions :: Core.Maybe [Types.AccessControlRuleAction],
    -- | IPv4 CIDR ranges to include in the rule.
    ipRanges :: Core.Maybe [Types.IpRange],
    -- | Access protocol actions to exclude from the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
    notActions :: Core.Maybe [Types.AccessControlRuleAction],
    -- | IPv4 CIDR ranges to exclude from the rule.
    notIpRanges :: Core.Maybe [Types.IpRange],
    -- | User IDs to exclude from the rule.
    notUserIds :: Core.Maybe [Types.WorkMailIdentifier],
    -- | User IDs to include in the rule.
    userIds :: Core.Maybe [Types.WorkMailIdentifier]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutAccessControlRule' value with any optional fields omitted.
mkPutAccessControlRule ::
  -- | 'name'
  Types.AccessControlRuleName ->
  -- | 'effect'
  Types.AccessControlRuleEffect ->
  -- | 'description'
  Types.AccessControlRuleDescription ->
  -- | 'organizationId'
  Types.OrganizationId ->
  PutAccessControlRule
mkPutAccessControlRule name effect description organizationId =
  PutAccessControlRule'
    { name,
      effect,
      description,
      organizationId,
      actions = Core.Nothing,
      ipRanges = Core.Nothing,
      notActions = Core.Nothing,
      notIpRanges = Core.Nothing,
      notUserIds = Core.Nothing,
      userIds = Core.Nothing
    }

-- | The rule name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrName :: Lens.Lens' PutAccessControlRule Types.AccessControlRuleName
pacrName = Lens.field @"name"
{-# DEPRECATED pacrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The rule effect.
--
-- /Note:/ Consider using 'effect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrEffect :: Lens.Lens' PutAccessControlRule Types.AccessControlRuleEffect
pacrEffect = Lens.field @"effect"
{-# DEPRECATED pacrEffect "Use generic-lens or generic-optics with 'effect' instead." #-}

-- | The rule description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrDescription :: Lens.Lens' PutAccessControlRule Types.AccessControlRuleDescription
pacrDescription = Lens.field @"description"
{-# DEPRECATED pacrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The identifier of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrOrganizationId :: Lens.Lens' PutAccessControlRule Types.OrganizationId
pacrOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED pacrOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | Access protocol actions to include in the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrActions :: Lens.Lens' PutAccessControlRule (Core.Maybe [Types.AccessControlRuleAction])
pacrActions = Lens.field @"actions"
{-# DEPRECATED pacrActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | IPv4 CIDR ranges to include in the rule.
--
-- /Note:/ Consider using 'ipRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrIpRanges :: Lens.Lens' PutAccessControlRule (Core.Maybe [Types.IpRange])
pacrIpRanges = Lens.field @"ipRanges"
{-# DEPRECATED pacrIpRanges "Use generic-lens or generic-optics with 'ipRanges' instead." #-}

-- | Access protocol actions to exclude from the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
--
-- /Note:/ Consider using 'notActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrNotActions :: Lens.Lens' PutAccessControlRule (Core.Maybe [Types.AccessControlRuleAction])
pacrNotActions = Lens.field @"notActions"
{-# DEPRECATED pacrNotActions "Use generic-lens or generic-optics with 'notActions' instead." #-}

-- | IPv4 CIDR ranges to exclude from the rule.
--
-- /Note:/ Consider using 'notIpRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrNotIpRanges :: Lens.Lens' PutAccessControlRule (Core.Maybe [Types.IpRange])
pacrNotIpRanges = Lens.field @"notIpRanges"
{-# DEPRECATED pacrNotIpRanges "Use generic-lens or generic-optics with 'notIpRanges' instead." #-}

-- | User IDs to exclude from the rule.
--
-- /Note:/ Consider using 'notUserIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrNotUserIds :: Lens.Lens' PutAccessControlRule (Core.Maybe [Types.WorkMailIdentifier])
pacrNotUserIds = Lens.field @"notUserIds"
{-# DEPRECATED pacrNotUserIds "Use generic-lens or generic-optics with 'notUserIds' instead." #-}

-- | User IDs to include in the rule.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrUserIds :: Lens.Lens' PutAccessControlRule (Core.Maybe [Types.WorkMailIdentifier])
pacrUserIds = Lens.field @"userIds"
{-# DEPRECATED pacrUserIds "Use generic-lens or generic-optics with 'userIds' instead." #-}

instance Core.FromJSON PutAccessControlRule where
  toJSON PutAccessControlRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Effect" Core..= effect),
            Core.Just ("Description" Core..= description),
            Core.Just ("OrganizationId" Core..= organizationId),
            ("Actions" Core..=) Core.<$> actions,
            ("IpRanges" Core..=) Core.<$> ipRanges,
            ("NotActions" Core..=) Core.<$> notActions,
            ("NotIpRanges" Core..=) Core.<$> notIpRanges,
            ("NotUserIds" Core..=) Core.<$> notUserIds,
            ("UserIds" Core..=) Core.<$> userIds
          ]
      )

instance Core.AWSRequest PutAccessControlRule where
  type Rs PutAccessControlRule = PutAccessControlRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "WorkMailService.PutAccessControlRule")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAccessControlRuleResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutAccessControlRuleResponse' smart constructor.
newtype PutAccessControlRuleResponse = PutAccessControlRuleResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutAccessControlRuleResponse' value with any optional fields omitted.
mkPutAccessControlRuleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutAccessControlRuleResponse
mkPutAccessControlRuleResponse responseStatus =
  PutAccessControlRuleResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrrrsResponseStatus :: Lens.Lens' PutAccessControlRuleResponse Core.Int
pacrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pacrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
