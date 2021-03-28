{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetApprovalRuleTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified approval rule template.
module Network.AWS.CodeCommit.GetApprovalRuleTemplate
    (
    -- * Creating a request
      GetApprovalRuleTemplate (..)
    , mkGetApprovalRuleTemplate
    -- ** Request lenses
    , gartApprovalRuleTemplateName

    -- * Destructuring the response
    , GetApprovalRuleTemplateResponse (..)
    , mkGetApprovalRuleTemplateResponse
    -- ** Response lenses
    , gartrrsApprovalRuleTemplate
    , gartrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetApprovalRuleTemplate' smart constructor.
newtype GetApprovalRuleTemplate = GetApprovalRuleTemplate'
  { approvalRuleTemplateName :: Types.ApprovalRuleTemplateName
    -- ^ The name of the approval rule template for which you want to get information.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetApprovalRuleTemplate' value with any optional fields omitted.
mkGetApprovalRuleTemplate
    :: Types.ApprovalRuleTemplateName -- ^ 'approvalRuleTemplateName'
    -> GetApprovalRuleTemplate
mkGetApprovalRuleTemplate approvalRuleTemplateName
  = GetApprovalRuleTemplate'{approvalRuleTemplateName}

-- | The name of the approval rule template for which you want to get information.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gartApprovalRuleTemplateName :: Lens.Lens' GetApprovalRuleTemplate Types.ApprovalRuleTemplateName
gartApprovalRuleTemplateName = Lens.field @"approvalRuleTemplateName"
{-# INLINEABLE gartApprovalRuleTemplateName #-}
{-# DEPRECATED approvalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead"  #-}

instance Core.ToQuery GetApprovalRuleTemplate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetApprovalRuleTemplate where
        toHeaders GetApprovalRuleTemplate{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.GetApprovalRuleTemplate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetApprovalRuleTemplate where
        toJSON GetApprovalRuleTemplate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("approvalRuleTemplateName" Core..= approvalRuleTemplateName)])

instance Core.AWSRequest GetApprovalRuleTemplate where
        type Rs GetApprovalRuleTemplate = GetApprovalRuleTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetApprovalRuleTemplateResponse' Core.<$>
                   (x Core..: "approvalRuleTemplate") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetApprovalRuleTemplateResponse' smart constructor.
data GetApprovalRuleTemplateResponse = GetApprovalRuleTemplateResponse'
  { approvalRuleTemplate :: Types.ApprovalRuleTemplate
    -- ^ The content and structure of the approval rule template.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetApprovalRuleTemplateResponse' value with any optional fields omitted.
mkGetApprovalRuleTemplateResponse
    :: Types.ApprovalRuleTemplate -- ^ 'approvalRuleTemplate'
    -> Core.Int -- ^ 'responseStatus'
    -> GetApprovalRuleTemplateResponse
mkGetApprovalRuleTemplateResponse approvalRuleTemplate
  responseStatus
  = GetApprovalRuleTemplateResponse'{approvalRuleTemplate,
                                     responseStatus}

-- | The content and structure of the approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gartrrsApprovalRuleTemplate :: Lens.Lens' GetApprovalRuleTemplateResponse Types.ApprovalRuleTemplate
gartrrsApprovalRuleTemplate = Lens.field @"approvalRuleTemplate"
{-# INLINEABLE gartrrsApprovalRuleTemplate #-}
{-# DEPRECATED approvalRuleTemplate "Use generic-lens or generic-optics with 'approvalRuleTemplate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gartrrsResponseStatus :: Lens.Lens' GetApprovalRuleTemplateResponse Core.Int
gartrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gartrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
