{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    EstimateTemplateCost (..),
    mkEstimateTemplateCost,

    -- ** Request lenses
    etcParameters,
    etcTemplateBody,
    etcTemplateURL,

    -- * Destructuring the response
    EstimateTemplateCostResponse (..),
    mkEstimateTemplateCostResponse,

    -- ** Response lenses
    etcrrsUrl,
    etcrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for an 'EstimateTemplateCost' action.
--
-- /See:/ 'mkEstimateTemplateCost' smart constructor.
data EstimateTemplateCost = EstimateTemplateCost'
  { -- | A list of @Parameter@ structures that specify input parameters.
    parameters :: Core.Maybe [Types.Parameter],
    -- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.)
    --
    -- Conditional: You must pass @TemplateBody@ or @TemplateURL@ . If both are passed, only @TemplateBody@ is used.
    templateBody :: Core.Maybe Types.TemplateBody,
    -- | Location of file containing the template body. The URL must point to a template that is located in an Amazon S3 bucket. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
    templateURL :: Core.Maybe Types.TemplateURL
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EstimateTemplateCost' value with any optional fields omitted.
mkEstimateTemplateCost ::
  EstimateTemplateCost
mkEstimateTemplateCost =
  EstimateTemplateCost'
    { parameters = Core.Nothing,
      templateBody = Core.Nothing,
      templateURL = Core.Nothing
    }

-- | A list of @Parameter@ structures that specify input parameters.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etcParameters :: Lens.Lens' EstimateTemplateCost (Core.Maybe [Types.Parameter])
etcParameters = Lens.field @"parameters"
{-# DEPRECATED etcParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. (For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.)
--
-- Conditional: You must pass @TemplateBody@ or @TemplateURL@ . If both are passed, only @TemplateBody@ is used.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etcTemplateBody :: Lens.Lens' EstimateTemplateCost (Core.Maybe Types.TemplateBody)
etcTemplateBody = Lens.field @"templateBody"
{-# DEPRECATED etcTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | Location of file containing the template body. The URL must point to a template that is located in an Amazon S3 bucket. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etcTemplateURL :: Lens.Lens' EstimateTemplateCost (Core.Maybe Types.TemplateURL)
etcTemplateURL = Lens.field @"templateURL"
{-# DEPRECATED etcTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

instance Core.AWSRequest EstimateTemplateCost where
  type Rs EstimateTemplateCost = EstimateTemplateCostResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "EstimateTemplateCost")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> ( Core.toQueryValue
                            "Parameters"
                            (Core.toQueryList "member" Core.<$> parameters)
                        )
                Core.<> (Core.toQueryValue "TemplateBody" Core.<$> templateBody)
                Core.<> (Core.toQueryValue "TemplateURL" Core.<$> templateURL)
            )
      }
  response =
    Response.receiveXMLWrapper
      "EstimateTemplateCostResult"
      ( \s h x ->
          EstimateTemplateCostResponse'
            Core.<$> (x Core..@? "Url") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output for a 'EstimateTemplateCost' action.
--
-- /See:/ 'mkEstimateTemplateCostResponse' smart constructor.
data EstimateTemplateCostResponse = EstimateTemplateCostResponse'
  { -- | An AWS Simple Monthly Calculator URL with a query string that describes the resources required to run the template.
    url :: Core.Maybe Types.Url,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EstimateTemplateCostResponse' value with any optional fields omitted.
mkEstimateTemplateCostResponse ::
  -- | 'responseStatus'
  Core.Int ->
  EstimateTemplateCostResponse
mkEstimateTemplateCostResponse responseStatus =
  EstimateTemplateCostResponse' {url = Core.Nothing, responseStatus}

-- | An AWS Simple Monthly Calculator URL with a query string that describes the resources required to run the template.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etcrrsUrl :: Lens.Lens' EstimateTemplateCostResponse (Core.Maybe Types.Url)
etcrrsUrl = Lens.field @"url"
{-# DEPRECATED etcrrsUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etcrrsResponseStatus :: Lens.Lens' EstimateTemplateCostResponse Core.Int
etcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED etcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
