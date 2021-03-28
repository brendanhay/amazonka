{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      PutAccessControlRule (..)
    , mkPutAccessControlRule
    -- ** Request lenses
    , pacrName
    , pacrEffect
    , pacrDescription
    , pacrOrganizationId
    , pacrActions
    , pacrIpRanges
    , pacrNotActions
    , pacrNotIpRanges
    , pacrNotUserIds
    , pacrUserIds

    -- * Destructuring the response
    , PutAccessControlRuleResponse (..)
    , mkPutAccessControlRuleResponse
    -- ** Response lenses
    , pacrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkPutAccessControlRule' smart constructor.
data PutAccessControlRule = PutAccessControlRule'
  { name :: Types.AccessControlRuleName
    -- ^ The rule name.
  , effect :: Types.AccessControlRuleEffect
    -- ^ The rule effect.
  , description :: Types.AccessControlRuleDescription
    -- ^ The rule description.
  , organizationId :: Types.OrganizationId
    -- ^ The identifier of the organization.
  , actions :: Core.Maybe [Types.AccessControlRuleAction]
    -- ^ Access protocol actions to include in the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
  , ipRanges :: Core.Maybe [Types.IpRange]
    -- ^ IPv4 CIDR ranges to include in the rule.
  , notActions :: Core.Maybe [Types.AccessControlRuleAction]
    -- ^ Access protocol actions to exclude from the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
  , notIpRanges :: Core.Maybe [Types.IpRange]
    -- ^ IPv4 CIDR ranges to exclude from the rule.
  , notUserIds :: Core.Maybe [Types.WorkMailIdentifier]
    -- ^ User IDs to exclude from the rule.
  , userIds :: Core.Maybe [Types.WorkMailIdentifier]
    -- ^ User IDs to include in the rule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutAccessControlRule' value with any optional fields omitted.
mkPutAccessControlRule
    :: Types.AccessControlRuleName -- ^ 'name'
    -> Types.AccessControlRuleEffect -- ^ 'effect'
    -> Types.AccessControlRuleDescription -- ^ 'description'
    -> Types.OrganizationId -- ^ 'organizationId'
    -> PutAccessControlRule
mkPutAccessControlRule name effect description organizationId
  = PutAccessControlRule'{name, effect, description, organizationId,
                          actions = Core.Nothing, ipRanges = Core.Nothing,
                          notActions = Core.Nothing, notIpRanges = Core.Nothing,
                          notUserIds = Core.Nothing, userIds = Core.Nothing}

-- | The rule name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrName :: Lens.Lens' PutAccessControlRule Types.AccessControlRuleName
pacrName = Lens.field @"name"
{-# INLINEABLE pacrName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The rule effect.
--
-- /Note:/ Consider using 'effect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrEffect :: Lens.Lens' PutAccessControlRule Types.AccessControlRuleEffect
pacrEffect = Lens.field @"effect"
{-# INLINEABLE pacrEffect #-}
{-# DEPRECATED effect "Use generic-lens or generic-optics with 'effect' instead"  #-}

-- | The rule description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrDescription :: Lens.Lens' PutAccessControlRule Types.AccessControlRuleDescription
pacrDescription = Lens.field @"description"
{-# INLINEABLE pacrDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The identifier of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrOrganizationId :: Lens.Lens' PutAccessControlRule Types.OrganizationId
pacrOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE pacrOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | Access protocol actions to include in the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrActions :: Lens.Lens' PutAccessControlRule (Core.Maybe [Types.AccessControlRuleAction])
pacrActions = Lens.field @"actions"
{-# INLINEABLE pacrActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

-- | IPv4 CIDR ranges to include in the rule.
--
-- /Note:/ Consider using 'ipRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrIpRanges :: Lens.Lens' PutAccessControlRule (Core.Maybe [Types.IpRange])
pacrIpRanges = Lens.field @"ipRanges"
{-# INLINEABLE pacrIpRanges #-}
{-# DEPRECATED ipRanges "Use generic-lens or generic-optics with 'ipRanges' instead"  #-}

-- | Access protocol actions to exclude from the rule. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
--
-- /Note:/ Consider using 'notActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrNotActions :: Lens.Lens' PutAccessControlRule (Core.Maybe [Types.AccessControlRuleAction])
pacrNotActions = Lens.field @"notActions"
{-# INLINEABLE pacrNotActions #-}
{-# DEPRECATED notActions "Use generic-lens or generic-optics with 'notActions' instead"  #-}

-- | IPv4 CIDR ranges to exclude from the rule.
--
-- /Note:/ Consider using 'notIpRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrNotIpRanges :: Lens.Lens' PutAccessControlRule (Core.Maybe [Types.IpRange])
pacrNotIpRanges = Lens.field @"notIpRanges"
{-# INLINEABLE pacrNotIpRanges #-}
{-# DEPRECATED notIpRanges "Use generic-lens or generic-optics with 'notIpRanges' instead"  #-}

-- | User IDs to exclude from the rule.
--
-- /Note:/ Consider using 'notUserIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrNotUserIds :: Lens.Lens' PutAccessControlRule (Core.Maybe [Types.WorkMailIdentifier])
pacrNotUserIds = Lens.field @"notUserIds"
{-# INLINEABLE pacrNotUserIds #-}
{-# DEPRECATED notUserIds "Use generic-lens or generic-optics with 'notUserIds' instead"  #-}

-- | User IDs to include in the rule.
--
-- /Note:/ Consider using 'userIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrUserIds :: Lens.Lens' PutAccessControlRule (Core.Maybe [Types.WorkMailIdentifier])
pacrUserIds = Lens.field @"userIds"
{-# INLINEABLE pacrUserIds #-}
{-# DEPRECATED userIds "Use generic-lens or generic-optics with 'userIds' instead"  #-}

instance Core.ToQuery PutAccessControlRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutAccessControlRule where
        toHeaders PutAccessControlRule{..}
          = Core.pure
              ("X-Amz-Target", "WorkMailService.PutAccessControlRule")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutAccessControlRule where
        toJSON PutAccessControlRule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("Effect" Core..= effect),
                  Core.Just ("Description" Core..= description),
                  Core.Just ("OrganizationId" Core..= organizationId),
                  ("Actions" Core..=) Core.<$> actions,
                  ("IpRanges" Core..=) Core.<$> ipRanges,
                  ("NotActions" Core..=) Core.<$> notActions,
                  ("NotIpRanges" Core..=) Core.<$> notIpRanges,
                  ("NotUserIds" Core..=) Core.<$> notUserIds,
                  ("UserIds" Core..=) Core.<$> userIds])

instance Core.AWSRequest PutAccessControlRule where
        type Rs PutAccessControlRule = PutAccessControlRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 PutAccessControlRuleResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutAccessControlRuleResponse' smart constructor.
newtype PutAccessControlRuleResponse = PutAccessControlRuleResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutAccessControlRuleResponse' value with any optional fields omitted.
mkPutAccessControlRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutAccessControlRuleResponse
mkPutAccessControlRuleResponse responseStatus
  = PutAccessControlRuleResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pacrrrsResponseStatus :: Lens.Lens' PutAccessControlRuleResponse Core.Int
pacrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pacrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
