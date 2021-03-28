{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateApprovalRuleTemplateName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name of a specified approval rule template.
module Network.AWS.CodeCommit.UpdateApprovalRuleTemplateName
    (
    -- * Creating a request
      UpdateApprovalRuleTemplateName (..)
    , mkUpdateApprovalRuleTemplateName
    -- ** Request lenses
    , uartnOldApprovalRuleTemplateName
    , uartnNewApprovalRuleTemplateName

    -- * Destructuring the response
    , UpdateApprovalRuleTemplateNameResponse (..)
    , mkUpdateApprovalRuleTemplateNameResponse
    -- ** Response lenses
    , uartnrrsApprovalRuleTemplate
    , uartnrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateApprovalRuleTemplateName' smart constructor.
data UpdateApprovalRuleTemplateName = UpdateApprovalRuleTemplateName'
  { oldApprovalRuleTemplateName :: Types.ApprovalRuleTemplateName
    -- ^ The current name of the approval rule template.
  , newApprovalRuleTemplateName :: Types.ApprovalRuleTemplateName
    -- ^ The new name you want to apply to the approval rule template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApprovalRuleTemplateName' value with any optional fields omitted.
mkUpdateApprovalRuleTemplateName
    :: Types.ApprovalRuleTemplateName -- ^ 'oldApprovalRuleTemplateName'
    -> Types.ApprovalRuleTemplateName -- ^ 'newApprovalRuleTemplateName'
    -> UpdateApprovalRuleTemplateName
mkUpdateApprovalRuleTemplateName oldApprovalRuleTemplateName
  newApprovalRuleTemplateName
  = UpdateApprovalRuleTemplateName'{oldApprovalRuleTemplateName,
                                    newApprovalRuleTemplateName}

-- | The current name of the approval rule template.
--
-- /Note:/ Consider using 'oldApprovalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartnOldApprovalRuleTemplateName :: Lens.Lens' UpdateApprovalRuleTemplateName Types.ApprovalRuleTemplateName
uartnOldApprovalRuleTemplateName = Lens.field @"oldApprovalRuleTemplateName"
{-# INLINEABLE uartnOldApprovalRuleTemplateName #-}
{-# DEPRECATED oldApprovalRuleTemplateName "Use generic-lens or generic-optics with 'oldApprovalRuleTemplateName' instead"  #-}

-- | The new name you want to apply to the approval rule template.
--
-- /Note:/ Consider using 'newApprovalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartnNewApprovalRuleTemplateName :: Lens.Lens' UpdateApprovalRuleTemplateName Types.ApprovalRuleTemplateName
uartnNewApprovalRuleTemplateName = Lens.field @"newApprovalRuleTemplateName"
{-# INLINEABLE uartnNewApprovalRuleTemplateName #-}
{-# DEPRECATED newApprovalRuleTemplateName "Use generic-lens or generic-optics with 'newApprovalRuleTemplateName' instead"  #-}

instance Core.ToQuery UpdateApprovalRuleTemplateName where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateApprovalRuleTemplateName where
        toHeaders UpdateApprovalRuleTemplateName{..}
          = Core.pure
              ("X-Amz-Target",
               "CodeCommit_20150413.UpdateApprovalRuleTemplateName")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateApprovalRuleTemplateName where
        toJSON UpdateApprovalRuleTemplateName{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("oldApprovalRuleTemplateName" Core..=
                       oldApprovalRuleTemplateName),
                  Core.Just
                    ("newApprovalRuleTemplateName" Core..=
                       newApprovalRuleTemplateName)])

instance Core.AWSRequest UpdateApprovalRuleTemplateName where
        type Rs UpdateApprovalRuleTemplateName =
             UpdateApprovalRuleTemplateNameResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateApprovalRuleTemplateNameResponse' Core.<$>
                   (x Core..: "approvalRuleTemplate") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateApprovalRuleTemplateNameResponse' smart constructor.
data UpdateApprovalRuleTemplateNameResponse = UpdateApprovalRuleTemplateNameResponse'
  { approvalRuleTemplate :: Types.ApprovalRuleTemplate
    -- ^ The structure and content of the updated approval rule template.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateApprovalRuleTemplateNameResponse' value with any optional fields omitted.
mkUpdateApprovalRuleTemplateNameResponse
    :: Types.ApprovalRuleTemplate -- ^ 'approvalRuleTemplate'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateApprovalRuleTemplateNameResponse
mkUpdateApprovalRuleTemplateNameResponse approvalRuleTemplate
  responseStatus
  = UpdateApprovalRuleTemplateNameResponse'{approvalRuleTemplate,
                                            responseStatus}

-- | The structure and content of the updated approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartnrrsApprovalRuleTemplate :: Lens.Lens' UpdateApprovalRuleTemplateNameResponse Types.ApprovalRuleTemplate
uartnrrsApprovalRuleTemplate = Lens.field @"approvalRuleTemplate"
{-# INLINEABLE uartnrrsApprovalRuleTemplate #-}
{-# DEPRECATED approvalRuleTemplate "Use generic-lens or generic-optics with 'approvalRuleTemplate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartnrrsResponseStatus :: Lens.Lens' UpdateApprovalRuleTemplateNameResponse Core.Int
uartnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uartnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
