{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetTemplateSummary (..)
    , mkGetTemplateSummary
    -- ** Request lenses
    , gtsStackName
    , gtsStackSetName
    , gtsTemplateBody
    , gtsTemplateURL

    -- * Destructuring the response
    , GetTemplateSummaryResponse (..)
    , mkGetTemplateSummaryResponse
    -- ** Response lenses
    , gtsrrsCapabilities
    , gtsrrsCapabilitiesReason
    , gtsrrsDeclaredTransforms
    , gtsrrsDescription
    , gtsrrsMetadata
    , gtsrrsParameters
    , gtsrrsResourceIdentifierSummaries
    , gtsrrsResourceTypes
    , gtsrrsVersion
    , gtsrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'GetTemplateSummary' action.
--
-- /See:/ 'mkGetTemplateSummary' smart constructor.
data GetTemplateSummary = GetTemplateSummary'
  { stackName :: Core.Maybe Types.StackName
    -- ^ The name or the stack ID that is associated with the stack, which are not always interchangeable. For running stacks, you can specify either the stack's name or its unique stack ID. For deleted stack, you must specify the unique stack ID.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
  , stackSetName :: Core.Maybe Types.StackSetName
    -- ^ The name or unique ID of the stack set from which the stack was created.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
  , templateBody :: Core.Maybe Types.TemplateBody
    -- ^ Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information about templates, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
  , templateURL :: Core.Maybe Types.TemplateURL
    -- ^ Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information about templates, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTemplateSummary' value with any optional fields omitted.
mkGetTemplateSummary
    :: GetTemplateSummary
mkGetTemplateSummary
  = GetTemplateSummary'{stackName = Core.Nothing,
                        stackSetName = Core.Nothing, templateBody = Core.Nothing,
                        templateURL = Core.Nothing}

-- | The name or the stack ID that is associated with the stack, which are not always interchangeable. For running stacks, you can specify either the stack's name or its unique stack ID. For deleted stack, you must specify the unique stack ID.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsStackName :: Lens.Lens' GetTemplateSummary (Core.Maybe Types.StackName)
gtsStackName = Lens.field @"stackName"
{-# INLINEABLE gtsStackName #-}
{-# DEPRECATED stackName "Use generic-lens or generic-optics with 'stackName' instead"  #-}

-- | The name or unique ID of the stack set from which the stack was created.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsStackSetName :: Lens.Lens' GetTemplateSummary (Core.Maybe Types.StackSetName)
gtsStackSetName = Lens.field @"stackSetName"
{-# INLINEABLE gtsStackSetName #-}
{-# DEPRECATED stackSetName "Use generic-lens or generic-optics with 'stackSetName' instead"  #-}

-- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information about templates, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsTemplateBody :: Lens.Lens' GetTemplateSummary (Core.Maybe Types.TemplateBody)
gtsTemplateBody = Lens.field @"templateBody"
{-# INLINEABLE gtsTemplateBody #-}
{-# DEPRECATED templateBody "Use generic-lens or generic-optics with 'templateBody' instead"  #-}

-- | Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information about templates, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsTemplateURL :: Lens.Lens' GetTemplateSummary (Core.Maybe Types.TemplateURL)
gtsTemplateURL = Lens.field @"templateURL"
{-# INLINEABLE gtsTemplateURL #-}
{-# DEPRECATED templateURL "Use generic-lens or generic-optics with 'templateURL' instead"  #-}

instance Core.ToQuery GetTemplateSummary where
        toQuery GetTemplateSummary{..}
          = Core.toQueryPair "Action" ("GetTemplateSummary" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-05-15" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StackName") stackName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StackSetName")
                stackSetName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TemplateBody")
                templateBody
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TemplateURL") templateURL

instance Core.ToHeaders GetTemplateSummary where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetTemplateSummary where
        type Rs GetTemplateSummary = GetTemplateSummaryResponse
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
          = Response.receiveXMLWrapper "GetTemplateSummaryResult"
              (\ s h x ->
                 GetTemplateSummaryResponse' Core.<$>
                   (x Core..@? "Capabilities" Core..<@> Core.parseXMLList "member")
                     Core.<*> x Core..@? "CapabilitiesReason"
                     Core.<*>
                     x Core..@? "DeclaredTransforms" Core..<@>
                       Core.parseXMLList "member"
                     Core.<*> x Core..@? "Description"
                     Core.<*> x Core..@? "Metadata"
                     Core.<*>
                     x Core..@? "Parameters" Core..<@> Core.parseXMLList "member"
                     Core.<*>
                     x Core..@? "ResourceIdentifierSummaries" Core..<@>
                       Core.parseXMLList "member"
                     Core.<*>
                     x Core..@? "ResourceTypes" Core..<@> Core.parseXMLList "member"
                     Core.<*> x Core..@? "Version"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output for the 'GetTemplateSummary' action.
--
-- /See:/ 'mkGetTemplateSummaryResponse' smart constructor.
data GetTemplateSummaryResponse = GetTemplateSummaryResponse'
  { capabilities :: Core.Maybe [Types.Capability]
    -- ^ The capabilities found within the template. If your template contains IAM resources, you must specify the CAPABILITY_IAM or CAPABILITY_NAMED_IAM value for this parameter when you use the 'CreateStack' or 'UpdateStack' actions with your template; otherwise, those actions return an InsufficientCapabilities error.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
  , capabilitiesReason :: Core.Maybe Types.CapabilitiesReason
    -- ^ The list of resources that generated the values in the @Capabilities@ response element.
  , declaredTransforms :: Core.Maybe [Types.TransformName]
    -- ^ A list of the transforms that are declared in the template.
  , description :: Core.Maybe Types.Description
    -- ^ The value that is defined in the @Description@ property of the template.
  , metadata :: Core.Maybe Types.Metadata
    -- ^ The value that is defined for the @Metadata@ property of the template.
  , parameters :: Core.Maybe [Types.ParameterDeclaration]
    -- ^ A list of parameter declarations that describe various properties for each parameter.
  , resourceIdentifierSummaries :: Core.Maybe [Types.ResourceIdentifierSummary]
    -- ^ A list of resource identifier summaries that describe the target resources of an import operation and the properties you can provide during the import to identify the target resources. For example, @BucketName@ is a possible identifier property for an @AWS::S3::Bucket@ resource. 
  , resourceTypes :: Core.Maybe [Types.ResourceType]
    -- ^ A list of all the template resource types that are defined in the template, such as @AWS::EC2::Instance@ , @AWS::Dynamo::Table@ , and @Custom::MyCustomInstance@ .
  , version :: Core.Maybe Types.Version
    -- ^ The AWS template format version, which identifies the capabilities of the template.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetTemplateSummaryResponse' value with any optional fields omitted.
mkGetTemplateSummaryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTemplateSummaryResponse
mkGetTemplateSummaryResponse responseStatus
  = GetTemplateSummaryResponse'{capabilities = Core.Nothing,
                                capabilitiesReason = Core.Nothing,
                                declaredTransforms = Core.Nothing, description = Core.Nothing,
                                metadata = Core.Nothing, parameters = Core.Nothing,
                                resourceIdentifierSummaries = Core.Nothing,
                                resourceTypes = Core.Nothing, version = Core.Nothing,
                                responseStatus}

-- | The capabilities found within the template. If your template contains IAM resources, you must specify the CAPABILITY_IAM or CAPABILITY_NAMED_IAM value for this parameter when you use the 'CreateStack' or 'UpdateStack' actions with your template; otherwise, those actions return an InsufficientCapabilities error.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsCapabilities :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe [Types.Capability])
gtsrrsCapabilities = Lens.field @"capabilities"
{-# INLINEABLE gtsrrsCapabilities #-}
{-# DEPRECATED capabilities "Use generic-lens or generic-optics with 'capabilities' instead"  #-}

-- | The list of resources that generated the values in the @Capabilities@ response element.
--
-- /Note:/ Consider using 'capabilitiesReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsCapabilitiesReason :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe Types.CapabilitiesReason)
gtsrrsCapabilitiesReason = Lens.field @"capabilitiesReason"
{-# INLINEABLE gtsrrsCapabilitiesReason #-}
{-# DEPRECATED capabilitiesReason "Use generic-lens or generic-optics with 'capabilitiesReason' instead"  #-}

-- | A list of the transforms that are declared in the template.
--
-- /Note:/ Consider using 'declaredTransforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsDeclaredTransforms :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe [Types.TransformName])
gtsrrsDeclaredTransforms = Lens.field @"declaredTransforms"
{-# INLINEABLE gtsrrsDeclaredTransforms #-}
{-# DEPRECATED declaredTransforms "Use generic-lens or generic-optics with 'declaredTransforms' instead"  #-}

-- | The value that is defined in the @Description@ property of the template.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsDescription :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe Types.Description)
gtsrrsDescription = Lens.field @"description"
{-# INLINEABLE gtsrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The value that is defined for the @Metadata@ property of the template.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsMetadata :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe Types.Metadata)
gtsrrsMetadata = Lens.field @"metadata"
{-# INLINEABLE gtsrrsMetadata #-}
{-# DEPRECATED metadata "Use generic-lens or generic-optics with 'metadata' instead"  #-}

-- | A list of parameter declarations that describe various properties for each parameter.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsParameters :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe [Types.ParameterDeclaration])
gtsrrsParameters = Lens.field @"parameters"
{-# INLINEABLE gtsrrsParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | A list of resource identifier summaries that describe the target resources of an import operation and the properties you can provide during the import to identify the target resources. For example, @BucketName@ is a possible identifier property for an @AWS::S3::Bucket@ resource. 
--
-- /Note:/ Consider using 'resourceIdentifierSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsResourceIdentifierSummaries :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe [Types.ResourceIdentifierSummary])
gtsrrsResourceIdentifierSummaries = Lens.field @"resourceIdentifierSummaries"
{-# INLINEABLE gtsrrsResourceIdentifierSummaries #-}
{-# DEPRECATED resourceIdentifierSummaries "Use generic-lens or generic-optics with 'resourceIdentifierSummaries' instead"  #-}

-- | A list of all the template resource types that are defined in the template, such as @AWS::EC2::Instance@ , @AWS::Dynamo::Table@ , and @Custom::MyCustomInstance@ .
--
-- /Note:/ Consider using 'resourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsResourceTypes :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe [Types.ResourceType])
gtsrrsResourceTypes = Lens.field @"resourceTypes"
{-# INLINEABLE gtsrrsResourceTypes #-}
{-# DEPRECATED resourceTypes "Use generic-lens or generic-optics with 'resourceTypes' instead"  #-}

-- | The AWS template format version, which identifies the capabilities of the template.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsVersion :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe Types.Version)
gtsrrsVersion = Lens.field @"version"
{-# INLINEABLE gtsrrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsResponseStatus :: Lens.Lens' GetTemplateSummaryResponse Core.Int
gtsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
