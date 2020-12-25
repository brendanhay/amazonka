{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.GetTemplateSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a new or existing template. The @GetTemplateSummary@ action is useful for viewing parameter information, such as default parameter values and parameter types, before you create or update a stack or stack set.
--
-- You can use the @GetTemplateSummary@ action when you submit a template, or you can get template information for a stack set, or a running or deleted stack.
-- For deleted stacks, @GetTemplateSummary@ returns the template information for up to 90 days after the stack has been deleted. If the template does not exist, a @ValidationError@ is returned.
module Network.AWS.CloudFormation.GetTemplateSummary
  ( -- * Creating a request
    GetTemplateSummary (..),
    mkGetTemplateSummary,

    -- ** Request lenses
    gtsStackName,
    gtsStackSetName,
    gtsTemplateBody,
    gtsTemplateURL,

    -- * Destructuring the response
    GetTemplateSummaryResponse (..),
    mkGetTemplateSummaryResponse,

    -- ** Response lenses
    gtsrrsCapabilities,
    gtsrrsCapabilitiesReason,
    gtsrrsDeclaredTransforms,
    gtsrrsDescription,
    gtsrrsMetadata,
    gtsrrsParameters,
    gtsrrsResourceIdentifierSummaries,
    gtsrrsResourceTypes,
    gtsrrsVersion,
    gtsrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'GetTemplateSummary' action.
--
-- /See:/ 'mkGetTemplateSummary' smart constructor.
data GetTemplateSummary = GetTemplateSummary'
  { -- | The name or the stack ID that is associated with the stack, which are not always interchangeable. For running stacks, you can specify either the stack's name or its unique stack ID. For deleted stack, you must specify the unique stack ID.
    --
    -- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
    stackName :: Core.Maybe Types.StackName,
    -- | The name or unique ID of the stack set from which the stack was created.
    --
    -- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
    stackSetName :: Core.Maybe Types.StackSetName,
    -- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information about templates, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
    templateBody :: Core.Maybe Types.TemplateBody,
    -- | Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information about templates, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
    templateURL :: Core.Maybe Types.TemplateURL
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTemplateSummary' value with any optional fields omitted.
mkGetTemplateSummary ::
  GetTemplateSummary
mkGetTemplateSummary =
  GetTemplateSummary'
    { stackName = Core.Nothing,
      stackSetName = Core.Nothing,
      templateBody = Core.Nothing,
      templateURL = Core.Nothing
    }

-- | The name or the stack ID that is associated with the stack, which are not always interchangeable. For running stacks, you can specify either the stack's name or its unique stack ID. For deleted stack, you must specify the unique stack ID.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsStackName :: Lens.Lens' GetTemplateSummary (Core.Maybe Types.StackName)
gtsStackName = Lens.field @"stackName"
{-# DEPRECATED gtsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | The name or unique ID of the stack set from which the stack was created.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsStackSetName :: Lens.Lens' GetTemplateSummary (Core.Maybe Types.StackSetName)
gtsStackSetName = Lens.field @"stackSetName"
{-# DEPRECATED gtsStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information about templates, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsTemplateBody :: Lens.Lens' GetTemplateSummary (Core.Maybe Types.TemplateBody)
gtsTemplateBody = Lens.field @"templateBody"
{-# DEPRECATED gtsTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information about templates, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsTemplateURL :: Lens.Lens' GetTemplateSummary (Core.Maybe Types.TemplateURL)
gtsTemplateURL = Lens.field @"templateURL"
{-# DEPRECATED gtsTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

instance Core.AWSRequest GetTemplateSummary where
  type Rs GetTemplateSummary = GetTemplateSummaryResponse
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
            ( Core.pure ("Action", "GetTemplateSummary")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackName" Core.<$> stackName)
                Core.<> (Core.toQueryValue "StackSetName" Core.<$> stackSetName)
                Core.<> (Core.toQueryValue "TemplateBody" Core.<$> templateBody)
                Core.<> (Core.toQueryValue "TemplateURL" Core.<$> templateURL)
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetTemplateSummaryResult"
      ( \s h x ->
          GetTemplateSummaryResponse'
            Core.<$> (x Core..@? "Capabilities" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "CapabilitiesReason")
            Core.<*> ( x Core..@? "DeclaredTransforms"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "Description")
            Core.<*> (x Core..@? "Metadata")
            Core.<*> (x Core..@? "Parameters" Core..<@> Core.parseXMLList "member")
            Core.<*> ( x Core..@? "ResourceIdentifierSummaries"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "ResourceTypes" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output for the 'GetTemplateSummary' action.
--
-- /See:/ 'mkGetTemplateSummaryResponse' smart constructor.
data GetTemplateSummaryResponse = GetTemplateSummaryResponse'
  { -- | The capabilities found within the template. If your template contains IAM resources, you must specify the CAPABILITY_IAM or CAPABILITY_NAMED_IAM value for this parameter when you use the 'CreateStack' or 'UpdateStack' actions with your template; otherwise, those actions return an InsufficientCapabilities error.
    --
    -- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
    capabilities :: Core.Maybe [Types.Capability],
    -- | The list of resources that generated the values in the @Capabilities@ response element.
    capabilitiesReason :: Core.Maybe Types.CapabilitiesReason,
    -- | A list of the transforms that are declared in the template.
    declaredTransforms :: Core.Maybe [Types.TransformName],
    -- | The value that is defined in the @Description@ property of the template.
    description :: Core.Maybe Types.Description,
    -- | The value that is defined for the @Metadata@ property of the template.
    metadata :: Core.Maybe Types.Metadata,
    -- | A list of parameter declarations that describe various properties for each parameter.
    parameters :: Core.Maybe [Types.ParameterDeclaration],
    -- | A list of resource identifier summaries that describe the target resources of an import operation and the properties you can provide during the import to identify the target resources. For example, @BucketName@ is a possible identifier property for an @AWS::S3::Bucket@ resource.
    resourceIdentifierSummaries :: Core.Maybe [Types.ResourceIdentifierSummary],
    -- | A list of all the template resource types that are defined in the template, such as @AWS::EC2::Instance@ , @AWS::Dynamo::Table@ , and @Custom::MyCustomInstance@ .
    resourceTypes :: Core.Maybe [Types.ResourceType],
    -- | The AWS template format version, which identifies the capabilities of the template.
    version :: Core.Maybe Types.Version,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTemplateSummaryResponse' value with any optional fields omitted.
mkGetTemplateSummaryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTemplateSummaryResponse
mkGetTemplateSummaryResponse responseStatus =
  GetTemplateSummaryResponse'
    { capabilities = Core.Nothing,
      capabilitiesReason = Core.Nothing,
      declaredTransforms = Core.Nothing,
      description = Core.Nothing,
      metadata = Core.Nothing,
      parameters = Core.Nothing,
      resourceIdentifierSummaries = Core.Nothing,
      resourceTypes = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The capabilities found within the template. If your template contains IAM resources, you must specify the CAPABILITY_IAM or CAPABILITY_NAMED_IAM value for this parameter when you use the 'CreateStack' or 'UpdateStack' actions with your template; otherwise, those actions return an InsufficientCapabilities error.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsCapabilities :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe [Types.Capability])
gtsrrsCapabilities = Lens.field @"capabilities"
{-# DEPRECATED gtsrrsCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | The list of resources that generated the values in the @Capabilities@ response element.
--
-- /Note:/ Consider using 'capabilitiesReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsCapabilitiesReason :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe Types.CapabilitiesReason)
gtsrrsCapabilitiesReason = Lens.field @"capabilitiesReason"
{-# DEPRECATED gtsrrsCapabilitiesReason "Use generic-lens or generic-optics with 'capabilitiesReason' instead." #-}

-- | A list of the transforms that are declared in the template.
--
-- /Note:/ Consider using 'declaredTransforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsDeclaredTransforms :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe [Types.TransformName])
gtsrrsDeclaredTransforms = Lens.field @"declaredTransforms"
{-# DEPRECATED gtsrrsDeclaredTransforms "Use generic-lens or generic-optics with 'declaredTransforms' instead." #-}

-- | The value that is defined in the @Description@ property of the template.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsDescription :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe Types.Description)
gtsrrsDescription = Lens.field @"description"
{-# DEPRECATED gtsrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The value that is defined for the @Metadata@ property of the template.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsMetadata :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe Types.Metadata)
gtsrrsMetadata = Lens.field @"metadata"
{-# DEPRECATED gtsrrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | A list of parameter declarations that describe various properties for each parameter.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsParameters :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe [Types.ParameterDeclaration])
gtsrrsParameters = Lens.field @"parameters"
{-# DEPRECATED gtsrrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A list of resource identifier summaries that describe the target resources of an import operation and the properties you can provide during the import to identify the target resources. For example, @BucketName@ is a possible identifier property for an @AWS::S3::Bucket@ resource.
--
-- /Note:/ Consider using 'resourceIdentifierSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsResourceIdentifierSummaries :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe [Types.ResourceIdentifierSummary])
gtsrrsResourceIdentifierSummaries = Lens.field @"resourceIdentifierSummaries"
{-# DEPRECATED gtsrrsResourceIdentifierSummaries "Use generic-lens or generic-optics with 'resourceIdentifierSummaries' instead." #-}

-- | A list of all the template resource types that are defined in the template, such as @AWS::EC2::Instance@ , @AWS::Dynamo::Table@ , and @Custom::MyCustomInstance@ .
--
-- /Note:/ Consider using 'resourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsResourceTypes :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe [Types.ResourceType])
gtsrrsResourceTypes = Lens.field @"resourceTypes"
{-# DEPRECATED gtsrrsResourceTypes "Use generic-lens or generic-optics with 'resourceTypes' instead." #-}

-- | The AWS template format version, which identifies the capabilities of the template.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsVersion :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe Types.Version)
gtsrrsVersion = Lens.field @"version"
{-# DEPRECATED gtsrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsResponseStatus :: Lens.Lens' GetTemplateSummaryResponse Core.Int
gtsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
