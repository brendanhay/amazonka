{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.CreateApprovalRuleTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a template for approval rules that can then be associated with one or more repositories in your AWS account. When you associate a template with a repository, AWS CodeCommit creates an approval rule that matches the conditions of the template for all pull requests that meet the conditions of the template. For more information, see 'AssociateApprovalRuleTemplateWithRepository' .
module Network.AWS.CodeCommit.CreateApprovalRuleTemplate
    (
    -- * Creating a request
      CreateApprovalRuleTemplate (..)
    , mkCreateApprovalRuleTemplate
    -- ** Request lenses
    , cartApprovalRuleTemplateName
    , cartApprovalRuleTemplateContent
    , cartApprovalRuleTemplateDescription

    -- * Destructuring the response
    , CreateApprovalRuleTemplateResponse (..)
    , mkCreateApprovalRuleTemplateResponse
    -- ** Response lenses
    , cartrrsApprovalRuleTemplate
    , cartrrsResponseStatus
    ) where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateApprovalRuleTemplate' smart constructor.
data CreateApprovalRuleTemplate = CreateApprovalRuleTemplate'
  { approvalRuleTemplateName :: Types.ApprovalRuleTemplateName
    -- ^ The name of the approval rule template. Provide descriptive names, because this name is applied to the approval rules created automatically in associated repositories.
  , approvalRuleTemplateContent :: Types.ApprovalRuleTemplateContent
    -- ^ The content of the approval rule that is created on pull requests in associated repositories. If you specify one or more destination references (branches), approval rules are created in an associated repository only if their destination references (branches) match those specified in the template.
  , approvalRuleTemplateDescription :: Core.Maybe Types.ApprovalRuleTemplateDescription
    -- ^ The description of the approval rule template. Consider providing a description that explains what this template does and when it might be appropriate to associate it with repositories.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApprovalRuleTemplate' value with any optional fields omitted.
mkCreateApprovalRuleTemplate
    :: Types.ApprovalRuleTemplateName -- ^ 'approvalRuleTemplateName'
    -> Types.ApprovalRuleTemplateContent -- ^ 'approvalRuleTemplateContent'
    -> CreateApprovalRuleTemplate
mkCreateApprovalRuleTemplate approvalRuleTemplateName
  approvalRuleTemplateContent
  = CreateApprovalRuleTemplate'{approvalRuleTemplateName,
                                approvalRuleTemplateContent,
                                approvalRuleTemplateDescription = Core.Nothing}

-- | The name of the approval rule template. Provide descriptive names, because this name is applied to the approval rules created automatically in associated repositories.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cartApprovalRuleTemplateName :: Lens.Lens' CreateApprovalRuleTemplate Types.ApprovalRuleTemplateName
cartApprovalRuleTemplateName = Lens.field @"approvalRuleTemplateName"
{-# INLINEABLE cartApprovalRuleTemplateName #-}
{-# DEPRECATED approvalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead"  #-}

-- | The content of the approval rule that is created on pull requests in associated repositories. If you specify one or more destination references (branches), approval rules are created in an associated repository only if their destination references (branches) match those specified in the template.
--
-- /Note:/ Consider using 'approvalRuleTemplateContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cartApprovalRuleTemplateContent :: Lens.Lens' CreateApprovalRuleTemplate Types.ApprovalRuleTemplateContent
cartApprovalRuleTemplateContent = Lens.field @"approvalRuleTemplateContent"
{-# INLINEABLE cartApprovalRuleTemplateContent #-}
{-# DEPRECATED approvalRuleTemplateContent "Use generic-lens or generic-optics with 'approvalRuleTemplateContent' instead"  #-}

-- | The description of the approval rule template. Consider providing a description that explains what this template does and when it might be appropriate to associate it with repositories.
--
-- /Note:/ Consider using 'approvalRuleTemplateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cartApprovalRuleTemplateDescription :: Lens.Lens' CreateApprovalRuleTemplate (Core.Maybe Types.ApprovalRuleTemplateDescription)
cartApprovalRuleTemplateDescription = Lens.field @"approvalRuleTemplateDescription"
{-# INLINEABLE cartApprovalRuleTemplateDescription #-}
{-# DEPRECATED approvalRuleTemplateDescription "Use generic-lens or generic-optics with 'approvalRuleTemplateDescription' instead"  #-}

instance Core.ToQuery CreateApprovalRuleTemplate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateApprovalRuleTemplate where
        toHeaders CreateApprovalRuleTemplate{..}
          = Core.pure
              ("X-Amz-Target", "CodeCommit_20150413.CreateApprovalRuleTemplate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateApprovalRuleTemplate where
        toJSON CreateApprovalRuleTemplate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("approvalRuleTemplateName" Core..= approvalRuleTemplateName),
                  Core.Just
                    ("approvalRuleTemplateContent" Core..=
                       approvalRuleTemplateContent),
                  ("approvalRuleTemplateDescription" Core..=) Core.<$>
                    approvalRuleTemplateDescription])

instance Core.AWSRequest CreateApprovalRuleTemplate where
        type Rs CreateApprovalRuleTemplate =
             CreateApprovalRuleTemplateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateApprovalRuleTemplateResponse' Core.<$>
                   (x Core..: "approvalRuleTemplate") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateApprovalRuleTemplateResponse' smart constructor.
data CreateApprovalRuleTemplateResponse = CreateApprovalRuleTemplateResponse'
  { approvalRuleTemplate :: Types.ApprovalRuleTemplate
    -- ^ The content and structure of the created approval rule template.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateApprovalRuleTemplateResponse' value with any optional fields omitted.
mkCreateApprovalRuleTemplateResponse
    :: Types.ApprovalRuleTemplate -- ^ 'approvalRuleTemplate'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateApprovalRuleTemplateResponse
mkCreateApprovalRuleTemplateResponse approvalRuleTemplate
  responseStatus
  = CreateApprovalRuleTemplateResponse'{approvalRuleTemplate,
                                        responseStatus}

-- | The content and structure of the created approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cartrrsApprovalRuleTemplate :: Lens.Lens' CreateApprovalRuleTemplateResponse Types.ApprovalRuleTemplate
cartrrsApprovalRuleTemplate = Lens.field @"approvalRuleTemplate"
{-# INLINEABLE cartrrsApprovalRuleTemplate #-}
{-# DEPRECATED approvalRuleTemplate "Use generic-lens or generic-optics with 'approvalRuleTemplate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cartrrsResponseStatus :: Lens.Lens' CreateApprovalRuleTemplateResponse Core.Int
cartrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cartrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
