{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.ListAccessControlRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the access control rules for the specified organization.
module Network.AWS.WorkMail.ListAccessControlRules
    (
    -- * Creating a request
      ListAccessControlRules (..)
    , mkListAccessControlRules
    -- ** Request lenses
    , lacrOrganizationId

    -- * Destructuring the response
    , ListAccessControlRulesResponse (..)
    , mkListAccessControlRulesResponse
    -- ** Response lenses
    , lacrrrsRules
    , lacrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkListAccessControlRules' smart constructor.
newtype ListAccessControlRules = ListAccessControlRules'
  { organizationId :: Types.OrganizationId
    -- ^ The identifier for the organization.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListAccessControlRules' value with any optional fields omitted.
mkListAccessControlRules
    :: Types.OrganizationId -- ^ 'organizationId'
    -> ListAccessControlRules
mkListAccessControlRules organizationId
  = ListAccessControlRules'{organizationId}

-- | The identifier for the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lacrOrganizationId :: Lens.Lens' ListAccessControlRules Types.OrganizationId
lacrOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE lacrOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

instance Core.ToQuery ListAccessControlRules where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListAccessControlRules where
        toHeaders ListAccessControlRules{..}
          = Core.pure
              ("X-Amz-Target", "WorkMailService.ListAccessControlRules")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListAccessControlRules where
        toJSON ListAccessControlRules{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("OrganizationId" Core..= organizationId)])

instance Core.AWSRequest ListAccessControlRules where
        type Rs ListAccessControlRules = ListAccessControlRulesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListAccessControlRulesResponse' Core.<$>
                   (x Core..:? "Rules") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListAccessControlRulesResponse' smart constructor.
data ListAccessControlRulesResponse = ListAccessControlRulesResponse'
  { rules :: Core.Maybe [Types.AccessControlRule]
    -- ^ The access control rules.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListAccessControlRulesResponse' value with any optional fields omitted.
mkListAccessControlRulesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAccessControlRulesResponse
mkListAccessControlRulesResponse responseStatus
  = ListAccessControlRulesResponse'{rules = Core.Nothing,
                                    responseStatus}

-- | The access control rules.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lacrrrsRules :: Lens.Lens' ListAccessControlRulesResponse (Core.Maybe [Types.AccessControlRule])
lacrrrsRules = Lens.field @"rules"
{-# INLINEABLE lacrrrsRules #-}
{-# DEPRECATED rules "Use generic-lens or generic-optics with 'rules' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lacrrrsResponseStatus :: Lens.Lens' ListAccessControlRulesResponse Core.Int
lacrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lacrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
