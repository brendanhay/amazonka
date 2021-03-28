{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.GetAccessControlEffect
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the effects of an organization's access control rules as they apply to a specified IPv4 address, access protocol action, or user ID. 
module Network.AWS.WorkMail.GetAccessControlEffect
    (
    -- * Creating a request
      GetAccessControlEffect (..)
    , mkGetAccessControlEffect
    -- ** Request lenses
    , gaceOrganizationId
    , gaceIpAddress
    , gaceAction
    , gaceUserId

    -- * Destructuring the response
    , GetAccessControlEffectResponse (..)
    , mkGetAccessControlEffectResponse
    -- ** Response lenses
    , gacerrsEffect
    , gacerrsMatchedRules
    , gacerrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkGetAccessControlEffect' smart constructor.
data GetAccessControlEffect = GetAccessControlEffect'
  { organizationId :: Types.OrganizationId
    -- ^ The identifier for the organization.
  , ipAddress :: Types.IpAddress
    -- ^ The IPv4 address.
  , action :: Types.AccessControlRuleAction
    -- ^ The access protocol action. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
  , userId :: Types.WorkMailIdentifier
    -- ^ The user ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAccessControlEffect' value with any optional fields omitted.
mkGetAccessControlEffect
    :: Types.OrganizationId -- ^ 'organizationId'
    -> Types.IpAddress -- ^ 'ipAddress'
    -> Types.AccessControlRuleAction -- ^ 'action'
    -> Types.WorkMailIdentifier -- ^ 'userId'
    -> GetAccessControlEffect
mkGetAccessControlEffect organizationId ipAddress action userId
  = GetAccessControlEffect'{organizationId, ipAddress, action,
                            userId}

-- | The identifier for the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaceOrganizationId :: Lens.Lens' GetAccessControlEffect Types.OrganizationId
gaceOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE gaceOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

-- | The IPv4 address.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaceIpAddress :: Lens.Lens' GetAccessControlEffect Types.IpAddress
gaceIpAddress = Lens.field @"ipAddress"
{-# INLINEABLE gaceIpAddress #-}
{-# DEPRECATED ipAddress "Use generic-lens or generic-optics with 'ipAddress' instead"  #-}

-- | The access protocol action. Valid values include @ActiveSync@ , @AutoDiscover@ , @EWS@ , @IMAP@ , @SMTP@ , @WindowsOutlook@ , and @WebMail@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaceAction :: Lens.Lens' GetAccessControlEffect Types.AccessControlRuleAction
gaceAction = Lens.field @"action"
{-# INLINEABLE gaceAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The user ID.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaceUserId :: Lens.Lens' GetAccessControlEffect Types.WorkMailIdentifier
gaceUserId = Lens.field @"userId"
{-# INLINEABLE gaceUserId #-}
{-# DEPRECATED userId "Use generic-lens or generic-optics with 'userId' instead"  #-}

instance Core.ToQuery GetAccessControlEffect where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAccessControlEffect where
        toHeaders GetAccessControlEffect{..}
          = Core.pure
              ("X-Amz-Target", "WorkMailService.GetAccessControlEffect")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetAccessControlEffect where
        toJSON GetAccessControlEffect{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId),
                  Core.Just ("IpAddress" Core..= ipAddress),
                  Core.Just ("Action" Core..= action),
                  Core.Just ("UserId" Core..= userId)])

instance Core.AWSRequest GetAccessControlEffect where
        type Rs GetAccessControlEffect = GetAccessControlEffectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAccessControlEffectResponse' Core.<$>
                   (x Core..:? "Effect") Core.<*> x Core..:? "MatchedRules" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAccessControlEffectResponse' smart constructor.
data GetAccessControlEffectResponse = GetAccessControlEffectResponse'
  { effect :: Core.Maybe Types.AccessControlRuleEffect
    -- ^ The rule effect.
  , matchedRules :: Core.Maybe [Types.AccessControlRuleName]
    -- ^ The rules that match the given parameters, resulting in an effect.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAccessControlEffectResponse' value with any optional fields omitted.
mkGetAccessControlEffectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAccessControlEffectResponse
mkGetAccessControlEffectResponse responseStatus
  = GetAccessControlEffectResponse'{effect = Core.Nothing,
                                    matchedRules = Core.Nothing, responseStatus}

-- | The rule effect.
--
-- /Note:/ Consider using 'effect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacerrsEffect :: Lens.Lens' GetAccessControlEffectResponse (Core.Maybe Types.AccessControlRuleEffect)
gacerrsEffect = Lens.field @"effect"
{-# INLINEABLE gacerrsEffect #-}
{-# DEPRECATED effect "Use generic-lens or generic-optics with 'effect' instead"  #-}

-- | The rules that match the given parameters, resulting in an effect.
--
-- /Note:/ Consider using 'matchedRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacerrsMatchedRules :: Lens.Lens' GetAccessControlEffectResponse (Core.Maybe [Types.AccessControlRuleName])
gacerrsMatchedRules = Lens.field @"matchedRules"
{-# INLINEABLE gacerrsMatchedRules #-}
{-# DEPRECATED matchedRules "Use generic-lens or generic-optics with 'matchedRules' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacerrsResponseStatus :: Lens.Lens' GetAccessControlEffectResponse Core.Int
gacerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gacerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
