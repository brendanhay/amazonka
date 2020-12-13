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
    gtsTemplateBody,
    gtsTemplateURL,
    gtsStackSetName,
    gtsStackName,

    -- * Destructuring the response
    GetTemplateSummaryResponse (..),
    mkGetTemplateSummaryResponse,

    -- ** Response lenses
    gtsrsDeclaredTransforms,
    gtsrsVersion,
    gtsrsCapabilitiesReason,
    gtsrsParameters,
    gtsrsMetadata,
    gtsrsResourceIdentifierSummaries,
    gtsrsDescription,
    gtsrsCapabilities,
    gtsrsResourceTypes,
    gtsrsResponseStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the 'GetTemplateSummary' action.
--
-- /See:/ 'mkGetTemplateSummary' smart constructor.
data GetTemplateSummary = GetTemplateSummary'
  { -- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information about templates, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
    templateBody :: Lude.Maybe Lude.Text,
    -- | Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information about templates, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
    templateURL :: Lude.Maybe Lude.Text,
    -- | The name or unique ID of the stack set from which the stack was created.
    --
    -- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
    stackSetName :: Lude.Maybe Lude.Text,
    -- | The name or the stack ID that is associated with the stack, which are not always interchangeable. For running stacks, you can specify either the stack's name or its unique stack ID. For deleted stack, you must specify the unique stack ID.
    --
    -- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
    stackName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTemplateSummary' with the minimum fields required to make a request.
--
-- * 'templateBody' - Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information about templates, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
-- * 'templateURL' - Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information about templates, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
-- * 'stackSetName' - The name or unique ID of the stack set from which the stack was created.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
-- * 'stackName' - The name or the stack ID that is associated with the stack, which are not always interchangeable. For running stacks, you can specify either the stack's name or its unique stack ID. For deleted stack, you must specify the unique stack ID.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
mkGetTemplateSummary ::
  GetTemplateSummary
mkGetTemplateSummary =
  GetTemplateSummary'
    { templateBody = Lude.Nothing,
      templateURL = Lude.Nothing,
      stackSetName = Lude.Nothing,
      stackName = Lude.Nothing
    }

-- | Structure containing the template body with a minimum length of 1 byte and a maximum length of 51,200 bytes. For more information about templates, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsTemplateBody :: Lens.Lens' GetTemplateSummary (Lude.Maybe Lude.Text)
gtsTemplateBody = Lens.lens (templateBody :: GetTemplateSummary -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: GetTemplateSummary)
{-# DEPRECATED gtsTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | Location of file containing the template body. The URL must point to a template (max size: 460,800 bytes) that is located in an Amazon S3 bucket. For more information about templates, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy> in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsTemplateURL :: Lens.Lens' GetTemplateSummary (Lude.Maybe Lude.Text)
gtsTemplateURL = Lens.lens (templateURL :: GetTemplateSummary -> Lude.Maybe Lude.Text) (\s a -> s {templateURL = a} :: GetTemplateSummary)
{-# DEPRECATED gtsTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

-- | The name or unique ID of the stack set from which the stack was created.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsStackSetName :: Lens.Lens' GetTemplateSummary (Lude.Maybe Lude.Text)
gtsStackSetName = Lens.lens (stackSetName :: GetTemplateSummary -> Lude.Maybe Lude.Text) (\s a -> s {stackSetName = a} :: GetTemplateSummary)
{-# DEPRECATED gtsStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | The name or the stack ID that is associated with the stack, which are not always interchangeable. For running stacks, you can specify either the stack's name or its unique stack ID. For deleted stack, you must specify the unique stack ID.
--
-- Conditional: You must specify only one of the following parameters: @StackName@ , @StackSetName@ , @TemplateBody@ , or @TemplateURL@ .
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsStackName :: Lens.Lens' GetTemplateSummary (Lude.Maybe Lude.Text)
gtsStackName = Lens.lens (stackName :: GetTemplateSummary -> Lude.Maybe Lude.Text) (\s a -> s {stackName = a} :: GetTemplateSummary)
{-# DEPRECATED gtsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest GetTemplateSummary where
  type Rs GetTemplateSummary = GetTemplateSummaryResponse
  request = Req.postQuery cloudFormationService
  response =
    Res.receiveXMLWrapper
      "GetTemplateSummaryResult"
      ( \s h x ->
          GetTemplateSummaryResponse'
            Lude.<$> ( x Lude..@? "DeclaredTransforms" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Version")
            Lude.<*> (x Lude..@? "CapabilitiesReason")
            Lude.<*> ( x Lude..@? "Parameters" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Metadata")
            Lude.<*> ( x Lude..@? "ResourceIdentifierSummaries" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Description")
            Lude.<*> ( x Lude..@? "Capabilities" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> ( x Lude..@? "ResourceTypes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTemplateSummary where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetTemplateSummary where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTemplateSummary where
  toQuery GetTemplateSummary' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetTemplateSummary" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-15" :: Lude.ByteString),
        "TemplateBody" Lude.=: templateBody,
        "TemplateURL" Lude.=: templateURL,
        "StackSetName" Lude.=: stackSetName,
        "StackName" Lude.=: stackName
      ]

-- | The output for the 'GetTemplateSummary' action.
--
-- /See:/ 'mkGetTemplateSummaryResponse' smart constructor.
data GetTemplateSummaryResponse = GetTemplateSummaryResponse'
  { -- | A list of the transforms that are declared in the template.
    declaredTransforms :: Lude.Maybe [Lude.Text],
    -- | The AWS template format version, which identifies the capabilities of the template.
    version :: Lude.Maybe Lude.Text,
    -- | The list of resources that generated the values in the @Capabilities@ response element.
    capabilitiesReason :: Lude.Maybe Lude.Text,
    -- | A list of parameter declarations that describe various properties for each parameter.
    parameters :: Lude.Maybe [ParameterDeclaration],
    -- | The value that is defined for the @Metadata@ property of the template.
    metadata :: Lude.Maybe Lude.Text,
    -- | A list of resource identifier summaries that describe the target resources of an import operation and the properties you can provide during the import to identify the target resources. For example, @BucketName@ is a possible identifier property for an @AWS::S3::Bucket@ resource.
    resourceIdentifierSummaries :: Lude.Maybe [ResourceIdentifierSummary],
    -- | The value that is defined in the @Description@ property of the template.
    description :: Lude.Maybe Lude.Text,
    -- | The capabilities found within the template. If your template contains IAM resources, you must specify the CAPABILITY_IAM or CAPABILITY_NAMED_IAM value for this parameter when you use the 'CreateStack' or 'UpdateStack' actions with your template; otherwise, those actions return an InsufficientCapabilities error.
    --
    -- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
    capabilities :: Lude.Maybe [Capability],
    -- | A list of all the template resource types that are defined in the template, such as @AWS::EC2::Instance@ , @AWS::Dynamo::Table@ , and @Custom::MyCustomInstance@ .
    resourceTypes :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTemplateSummaryResponse' with the minimum fields required to make a request.
--
-- * 'declaredTransforms' - A list of the transforms that are declared in the template.
-- * 'version' - The AWS template format version, which identifies the capabilities of the template.
-- * 'capabilitiesReason' - The list of resources that generated the values in the @Capabilities@ response element.
-- * 'parameters' - A list of parameter declarations that describe various properties for each parameter.
-- * 'metadata' - The value that is defined for the @Metadata@ property of the template.
-- * 'resourceIdentifierSummaries' - A list of resource identifier summaries that describe the target resources of an import operation and the properties you can provide during the import to identify the target resources. For example, @BucketName@ is a possible identifier property for an @AWS::S3::Bucket@ resource.
-- * 'description' - The value that is defined in the @Description@ property of the template.
-- * 'capabilities' - The capabilities found within the template. If your template contains IAM resources, you must specify the CAPABILITY_IAM or CAPABILITY_NAMED_IAM value for this parameter when you use the 'CreateStack' or 'UpdateStack' actions with your template; otherwise, those actions return an InsufficientCapabilities error.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
-- * 'resourceTypes' - A list of all the template resource types that are defined in the template, such as @AWS::EC2::Instance@ , @AWS::Dynamo::Table@ , and @Custom::MyCustomInstance@ .
-- * 'responseStatus' - The response status code.
mkGetTemplateSummaryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTemplateSummaryResponse
mkGetTemplateSummaryResponse pResponseStatus_ =
  GetTemplateSummaryResponse'
    { declaredTransforms = Lude.Nothing,
      version = Lude.Nothing,
      capabilitiesReason = Lude.Nothing,
      parameters = Lude.Nothing,
      metadata = Lude.Nothing,
      resourceIdentifierSummaries = Lude.Nothing,
      description = Lude.Nothing,
      capabilities = Lude.Nothing,
      resourceTypes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the transforms that are declared in the template.
--
-- /Note:/ Consider using 'declaredTransforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsDeclaredTransforms :: Lens.Lens' GetTemplateSummaryResponse (Lude.Maybe [Lude.Text])
gtsrsDeclaredTransforms = Lens.lens (declaredTransforms :: GetTemplateSummaryResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {declaredTransforms = a} :: GetTemplateSummaryResponse)
{-# DEPRECATED gtsrsDeclaredTransforms "Use generic-lens or generic-optics with 'declaredTransforms' instead." #-}

-- | The AWS template format version, which identifies the capabilities of the template.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsVersion :: Lens.Lens' GetTemplateSummaryResponse (Lude.Maybe Lude.Text)
gtsrsVersion = Lens.lens (version :: GetTemplateSummaryResponse -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: GetTemplateSummaryResponse)
{-# DEPRECATED gtsrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The list of resources that generated the values in the @Capabilities@ response element.
--
-- /Note:/ Consider using 'capabilitiesReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsCapabilitiesReason :: Lens.Lens' GetTemplateSummaryResponse (Lude.Maybe Lude.Text)
gtsrsCapabilitiesReason = Lens.lens (capabilitiesReason :: GetTemplateSummaryResponse -> Lude.Maybe Lude.Text) (\s a -> s {capabilitiesReason = a} :: GetTemplateSummaryResponse)
{-# DEPRECATED gtsrsCapabilitiesReason "Use generic-lens or generic-optics with 'capabilitiesReason' instead." #-}

-- | A list of parameter declarations that describe various properties for each parameter.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsParameters :: Lens.Lens' GetTemplateSummaryResponse (Lude.Maybe [ParameterDeclaration])
gtsrsParameters = Lens.lens (parameters :: GetTemplateSummaryResponse -> Lude.Maybe [ParameterDeclaration]) (\s a -> s {parameters = a} :: GetTemplateSummaryResponse)
{-# DEPRECATED gtsrsParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The value that is defined for the @Metadata@ property of the template.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsMetadata :: Lens.Lens' GetTemplateSummaryResponse (Lude.Maybe Lude.Text)
gtsrsMetadata = Lens.lens (metadata :: GetTemplateSummaryResponse -> Lude.Maybe Lude.Text) (\s a -> s {metadata = a} :: GetTemplateSummaryResponse)
{-# DEPRECATED gtsrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | A list of resource identifier summaries that describe the target resources of an import operation and the properties you can provide during the import to identify the target resources. For example, @BucketName@ is a possible identifier property for an @AWS::S3::Bucket@ resource.
--
-- /Note:/ Consider using 'resourceIdentifierSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsResourceIdentifierSummaries :: Lens.Lens' GetTemplateSummaryResponse (Lude.Maybe [ResourceIdentifierSummary])
gtsrsResourceIdentifierSummaries = Lens.lens (resourceIdentifierSummaries :: GetTemplateSummaryResponse -> Lude.Maybe [ResourceIdentifierSummary]) (\s a -> s {resourceIdentifierSummaries = a} :: GetTemplateSummaryResponse)
{-# DEPRECATED gtsrsResourceIdentifierSummaries "Use generic-lens or generic-optics with 'resourceIdentifierSummaries' instead." #-}

-- | The value that is defined in the @Description@ property of the template.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsDescription :: Lens.Lens' GetTemplateSummaryResponse (Lude.Maybe Lude.Text)
gtsrsDescription = Lens.lens (description :: GetTemplateSummaryResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GetTemplateSummaryResponse)
{-# DEPRECATED gtsrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The capabilities found within the template. If your template contains IAM resources, you must specify the CAPABILITY_IAM or CAPABILITY_NAMED_IAM value for this parameter when you use the 'CreateStack' or 'UpdateStack' actions with your template; otherwise, those actions return an InsufficientCapabilities error.
--
-- For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates> .
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsCapabilities :: Lens.Lens' GetTemplateSummaryResponse (Lude.Maybe [Capability])
gtsrsCapabilities = Lens.lens (capabilities :: GetTemplateSummaryResponse -> Lude.Maybe [Capability]) (\s a -> s {capabilities = a} :: GetTemplateSummaryResponse)
{-# DEPRECATED gtsrsCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | A list of all the template resource types that are defined in the template, such as @AWS::EC2::Instance@ , @AWS::Dynamo::Table@ , and @Custom::MyCustomInstance@ .
--
-- /Note:/ Consider using 'resourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsResourceTypes :: Lens.Lens' GetTemplateSummaryResponse (Lude.Maybe [Lude.Text])
gtsrsResourceTypes = Lens.lens (resourceTypes :: GetTemplateSummaryResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceTypes = a} :: GetTemplateSummaryResponse)
{-# DEPRECATED gtsrsResourceTypes "Use generic-lens or generic-optics with 'resourceTypes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsResponseStatus :: Lens.Lens' GetTemplateSummaryResponse Lude.Int
gtsrsResponseStatus = Lens.lens (responseStatus :: GetTemplateSummaryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTemplateSummaryResponse)
{-# DEPRECATED gtsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
