{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.EstimateTemplateCost
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the estimated monthly cost of a template. The return value is an AWS Simple Monthly Calculator URL with a query string that describes the resources required to run the template.
module Network.AWS.CloudFormation.EstimateTemplateCost
    (
    -- * Creating a request
      EstimateTemplateCost (..)
    , mkEstimateTemplateCost
    -- ** Request lenses
    , etcParameters
    , etcTemplateBody
    , etcTemplateURL

    -- * Destructuring the response
    , EstimateTemplateCostResponse (..)
    , mkEstimateTemplateCostResponse
    -- ** Response lenses
    , etcrrsUrl
    , etcrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for an 'EstimateTemplateCost' action.
--
-- /See:/ 'mkEstimateTemplateCost' smart constructor.
data EstimateTemplateCost = EstimateTemplateCost'
  { parameters :: Core.Maybe [Types.Parameter]
    -- ^ A list of @Parameter@ structures that specify input parameters.
  , templateBody :: Core.Maybe Types.TemplateBody
    -- ^ Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.)
--
-- Conditional: You must pass @TemplateBody@ or @TemplateURL@ . If both are passed, only @TemplateBody@ is used.
  , templateURL :: Core.Maybe Types.TemplateURL
    -- ^ Location of file containing the template body. The URL must point to a template that is located in an Amazon S3 bucket. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EstimateTemplateCost' value with any optional fields omitted.
mkEstimateTemplateCost
    :: EstimateTemplateCost
mkEstimateTemplateCost
  = EstimateTemplateCost'{parameters = Core.Nothing,
                          templateBody = Core.Nothing, templateURL = Core.Nothing}

-- | A list of @Parameter@ structures that specify input parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etcParameters :: Lens.Lens' EstimateTemplateCost (Core.Maybe [Types.Parameter])
etcParameters = Lens.field @"parameters"
{-# INLINEABLE etcParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.)
--
-- Conditional: You must pass @TemplateBody@ or @TemplateURL@ . If both are passed, only @TemplateBody@ is used.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etcTemplateBody :: Lens.Lens' EstimateTemplateCost (Core.Maybe Types.TemplateBody)
etcTemplateBody = Lens.field @"templateBody"
{-# INLINEABLE etcTemplateBody #-}
{-# DEPRECATED templateBody "Use generic-lens or generic-optics with 'templateBody' instead"  #-}

-- | Location of file containing the template body. The URL must point to a template that is located in an Amazon S3 bucket. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etcTemplateURL :: Lens.Lens' EstimateTemplateCost (Core.Maybe Types.TemplateURL)
etcTemplateURL = Lens.field @"templateURL"
{-# INLINEABLE etcTemplateURL #-}
{-# DEPRECATED templateURL "Use generic-lens or generic-optics with 'templateURL' instead"  #-}

instance Core.ToQuery EstimateTemplateCost where
        toQuery EstimateTemplateCost{..}
          = Core.toQueryPair "Action" ("EstimateTemplateCost" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<>
              Core.toQueryPair "Parameters"
                (Core.maybe Core.mempty (Core.toQueryList "member") parameters)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TemplateBody")
                templateBody
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TemplateURL") templateURL

instance Core.ToHeaders EstimateTemplateCost where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest EstimateTemplateCost where
        type Rs EstimateTemplateCost = EstimateTemplateCostResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "EstimateTemplateCostResult"
              (\ s h x ->
                 EstimateTemplateCostResponse' Core.<$>
                   (x Core..@? "Url") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output for a 'EstimateTemplateCost' action.
--
-- /See:/ 'mkEstimateTemplateCostResponse' smart constructor.
data EstimateTemplateCostResponse = EstimateTemplateCostResponse'
  { url :: Core.Maybe Types.Url
    -- ^ An AWS Simple Monthly Calculator URL with a query string that describes the resources required to run the template.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EstimateTemplateCostResponse' value with any optional fields omitted.
mkEstimateTemplateCostResponse
    :: Core.Int -- ^ 'responseStatus'
    -> EstimateTemplateCostResponse
mkEstimateTemplateCostResponse responseStatus
  = EstimateTemplateCostResponse'{url = Core.Nothing, responseStatus}

-- | An AWS Simple Monthly Calculator URL with a query string that describes the resources required to run the template.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etcrrsUrl :: Lens.Lens' EstimateTemplateCostResponse (Core.Maybe Types.Url)
etcrrsUrl = Lens.field @"url"
{-# INLINEABLE etcrrsUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etcrrsResponseStatus :: Lens.Lens' EstimateTemplateCostResponse Core.Int
etcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE etcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
