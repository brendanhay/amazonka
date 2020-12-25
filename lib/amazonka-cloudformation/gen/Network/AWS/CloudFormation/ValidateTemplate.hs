{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ValidateTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates a specified template. AWS CloudFormation first checks if the template is valid JSON. If it isn't, AWS CloudFormation checks if the template is valid YAML. If both these checks fail, AWS CloudFormation returns a template validation error.
module Network.AWS.CloudFormation.ValidateTemplate
  ( -- * Creating a request
    ValidateTemplate (..),
    mkValidateTemplate,

    -- ** Request lenses
    vtTemplateBody,
    vtTemplateURL,

    -- * Destructuring the response
    ValidateTemplateResponse (..),
    mkValidateTemplateResponse,

    -- ** Response lenses
    vtrrsCapabilities,
    vtrrsCapabilitiesReason,
    vtrrsDeclaredTransforms,
    vtrrsDescription,
    vtrrsParameters,
    vtrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for 'ValidateTemplate' action.
--
-- /See:/ 'mkValidateTemplate' smart constructor.
data ValidateTemplate = ValidateTemplate'
  { -- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
    templateBody :: Core.Maybe Types.TemplateBody,
    -- | Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
    templateURL :: Core.Maybe Types.TemplateURL
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValidateTemplate' value with any optional fields omitted.
mkValidateTemplate ::
  ValidateTemplate
mkValidateTemplate =
  ValidateTemplate'
    { templateBody = Core.Nothing,
      templateURL = Core.Nothing
    }

-- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtTemplateBody :: Lens.Lens' ValidateTemplate (Core.Maybe Types.TemplateBody)
vtTemplateBody = Lens.field @"templateBody"
{-# DEPRECATED vtTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information, go to <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@ . If both are passed, only @TemplateBody@ is used.
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtTemplateURL :: Lens.Lens' ValidateTemplate (Core.Maybe Types.TemplateURL)
vtTemplateURL = Lens.field @"templateURL"
{-# DEPRECATED vtTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

instance Core.AWSRequest ValidateTemplate where
  type Rs ValidateTemplate = ValidateTemplateResponse
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
            ( Core.pure ("Action", "ValidateTemplate")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "TemplateBody" Core.<$> templateBody)
                Core.<> (Core.toQueryValue "TemplateURL" Core.<$> templateURL)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ValidateTemplateResult"
      ( \s h x ->
          ValidateTemplateResponse'
            Core.<$> (x Core..@? "Capabilities" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "CapabilitiesReason")
            Core.<*> ( x Core..@? "DeclaredTransforms"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "Description")
            Core.<*> (x Core..@? "Parameters" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output for 'ValidateTemplate' action.
--
-- /See:/ 'mkValidateTemplateResponse' smart constructor.
data ValidateTemplateResponse = ValidateTemplateResponse'
  { -- | The capabilities found within the template. If your template contains IAM resources, you must specify the CAPABILITY_IAM or CAPABILITY_NAMED_IAM value for this parameter when you use the 'CreateStack' or 'UpdateStack' actions with your template; otherwise, those actions return an InsufficientCapabilities error.
    --
    -- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
    capabilities :: Core.Maybe [Types.Capability],
    -- | The list of resources that generated the values in the @Capabilities@ response element.
    capabilitiesReason :: Core.Maybe Types.CapabilitiesReason,
    -- | A list of the transforms that are declared in the template.
    declaredTransforms :: Core.Maybe [Types.TransformName],
    -- | The description found within the template.
    description :: Core.Maybe Types.Description,
    -- | A list of @TemplateParameter@ structures.
    parameters :: Core.Maybe [Types.TemplateParameter],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValidateTemplateResponse' value with any optional fields omitted.
mkValidateTemplateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ValidateTemplateResponse
mkValidateTemplateResponse responseStatus =
  ValidateTemplateResponse'
    { capabilities = Core.Nothing,
      capabilitiesReason = Core.Nothing,
      declaredTransforms = Core.Nothing,
      description = Core.Nothing,
      parameters = Core.Nothing,
      responseStatus
    }

-- | The capabilities found within the template. If your template contains IAM resources, you must specify the CAPABILITY_IAM or CAPABILITY_NAMED_IAM value for this parameter when you use the 'CreateStack' or 'UpdateStack' actions with your template; otherwise, those actions return an InsufficientCapabilities error.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrrsCapabilities :: Lens.Lens' ValidateTemplateResponse (Core.Maybe [Types.Capability])
vtrrsCapabilities = Lens.field @"capabilities"
{-# DEPRECATED vtrrsCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | The list of resources that generated the values in the @Capabilities@ response element.
--
-- /Note:/ Consider using 'capabilitiesReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrrsCapabilitiesReason :: Lens.Lens' ValidateTemplateResponse (Core.Maybe Types.CapabilitiesReason)
vtrrsCapabilitiesReason = Lens.field @"capabilitiesReason"
{-# DEPRECATED vtrrsCapabilitiesReason "Use generic-lens or generic-optics with 'capabilitiesReason' instead." #-}

-- | A list of the transforms that are declared in the template.
--
-- /Note:/ Consider using 'declaredTransforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrrsDeclaredTransforms :: Lens.Lens' ValidateTemplateResponse (Core.Maybe [Types.TransformName])
vtrrsDeclaredTransforms = Lens.field @"declaredTransforms"
{-# DEPRECATED vtrrsDeclaredTransforms "Use generic-lens or generic-optics with 'declaredTransforms' instead." #-}

-- | The description found within the template.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrrsDescription :: Lens.Lens' ValidateTemplateResponse (Core.Maybe Types.Description)
vtrrsDescription = Lens.field @"description"
{-# DEPRECATED vtrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of @TemplateParameter@ structures.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrrsParameters :: Lens.Lens' ValidateTemplateResponse (Core.Maybe [Types.TemplateParameter])
vtrrsParameters = Lens.field @"parameters"
{-# DEPRECATED vtrrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vtrrsResponseStatus :: Lens.Lens' ValidateTemplateResponse Core.Int
vtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED vtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
