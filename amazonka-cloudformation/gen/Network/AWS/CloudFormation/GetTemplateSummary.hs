{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.GetTemplateSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a new or existing template. The
-- @GetTemplateSummary@ action is useful for viewing parameter information,
-- such as default parameter values and parameter types, before you create
-- or update a stack or stack set.
--
-- You can use the @GetTemplateSummary@ action when you submit a template,
-- or you can get template information for a stack set, or a running or
-- deleted stack.
--
-- For deleted stacks, @GetTemplateSummary@ returns the template
-- information for up to 90 days after the stack has been deleted. If the
-- template does not exist, a @ValidationError@ is returned.
module Network.AWS.CloudFormation.GetTemplateSummary
  ( -- * Creating a Request
    GetTemplateSummary (..),
    newGetTemplateSummary,

    -- * Request Lenses
    getTemplateSummary_stackName,
    getTemplateSummary_templateURL,
    getTemplateSummary_stackSetName,
    getTemplateSummary_templateBody,

    -- * Destructuring the Response
    GetTemplateSummaryResponse (..),
    newGetTemplateSummaryResponse,

    -- * Response Lenses
    getTemplateSummaryResponse_resourceTypes,
    getTemplateSummaryResponse_capabilities,
    getTemplateSummaryResponse_resourceIdentifierSummaries,
    getTemplateSummaryResponse_declaredTransforms,
    getTemplateSummaryResponse_metadata,
    getTemplateSummaryResponse_version,
    getTemplateSummaryResponse_description,
    getTemplateSummaryResponse_parameters,
    getTemplateSummaryResponse_capabilitiesReason,
    getTemplateSummaryResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetTemplateSummary action.
--
-- /See:/ 'newGetTemplateSummary' smart constructor.
data GetTemplateSummary = GetTemplateSummary'
  { -- | The name or the stack ID that is associated with the stack, which are
    -- not always interchangeable. For running stacks, you can specify either
    -- the stack\'s name or its unique stack ID. For deleted stack, you must
    -- specify the unique stack ID.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
    stackName :: Core.Maybe Core.Text,
    -- | Location of file containing the template body. The URL must point to a
    -- template (max size: 460,800 bytes) that is located in an Amazon S3
    -- bucket or a Systems Manager document. For more information about
    -- templates, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
    templateURL :: Core.Maybe Core.Text,
    -- | The name or unique ID of the stack set from which the stack was created.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
    stackSetName :: Core.Maybe Core.Text,
    -- | Structure containing the template body with a minimum length of 1 byte
    -- and a maximum length of 51,200 bytes. For more information about
    -- templates, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
    templateBody :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTemplateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackName', 'getTemplateSummary_stackName' - The name or the stack ID that is associated with the stack, which are
-- not always interchangeable. For running stacks, you can specify either
-- the stack\'s name or its unique stack ID. For deleted stack, you must
-- specify the unique stack ID.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
--
-- 'templateURL', 'getTemplateSummary_templateURL' - Location of file containing the template body. The URL must point to a
-- template (max size: 460,800 bytes) that is located in an Amazon S3
-- bucket or a Systems Manager document. For more information about
-- templates, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
--
-- 'stackSetName', 'getTemplateSummary_stackSetName' - The name or unique ID of the stack set from which the stack was created.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
--
-- 'templateBody', 'getTemplateSummary_templateBody' - Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. For more information about
-- templates, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
newGetTemplateSummary ::
  GetTemplateSummary
newGetTemplateSummary =
  GetTemplateSummary'
    { stackName = Core.Nothing,
      templateURL = Core.Nothing,
      stackSetName = Core.Nothing,
      templateBody = Core.Nothing
    }

-- | The name or the stack ID that is associated with the stack, which are
-- not always interchangeable. For running stacks, you can specify either
-- the stack\'s name or its unique stack ID. For deleted stack, you must
-- specify the unique stack ID.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
getTemplateSummary_stackName :: Lens.Lens' GetTemplateSummary (Core.Maybe Core.Text)
getTemplateSummary_stackName = Lens.lens (\GetTemplateSummary' {stackName} -> stackName) (\s@GetTemplateSummary' {} a -> s {stackName = a} :: GetTemplateSummary)

-- | Location of file containing the template body. The URL must point to a
-- template (max size: 460,800 bytes) that is located in an Amazon S3
-- bucket or a Systems Manager document. For more information about
-- templates, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
getTemplateSummary_templateURL :: Lens.Lens' GetTemplateSummary (Core.Maybe Core.Text)
getTemplateSummary_templateURL = Lens.lens (\GetTemplateSummary' {templateURL} -> templateURL) (\s@GetTemplateSummary' {} a -> s {templateURL = a} :: GetTemplateSummary)

-- | The name or unique ID of the stack set from which the stack was created.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
getTemplateSummary_stackSetName :: Lens.Lens' GetTemplateSummary (Core.Maybe Core.Text)
getTemplateSummary_stackSetName = Lens.lens (\GetTemplateSummary' {stackSetName} -> stackSetName) (\s@GetTemplateSummary' {} a -> s {stackSetName = a} :: GetTemplateSummary)

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. For more information about
-- templates, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
getTemplateSummary_templateBody :: Lens.Lens' GetTemplateSummary (Core.Maybe Core.Text)
getTemplateSummary_templateBody = Lens.lens (\GetTemplateSummary' {templateBody} -> templateBody) (\s@GetTemplateSummary' {} a -> s {templateBody = a} :: GetTemplateSummary)

instance Core.AWSRequest GetTemplateSummary where
  type
    AWSResponse GetTemplateSummary =
      GetTemplateSummaryResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetTemplateSummaryResult"
      ( \s h x ->
          GetTemplateSummaryResponse'
            Core.<$> ( x Core..@? "ResourceTypes" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> ( x Core..@? "Capabilities" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> ( x Core..@? "ResourceIdentifierSummaries"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> ( x Core..@? "DeclaredTransforms" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "Metadata")
            Core.<*> (x Core..@? "Version")
            Core.<*> (x Core..@? "Description")
            Core.<*> ( x Core..@? "Parameters" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "CapabilitiesReason")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTemplateSummary

instance Core.NFData GetTemplateSummary

instance Core.ToHeaders GetTemplateSummary where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetTemplateSummary where
  toPath = Core.const "/"

instance Core.ToQuery GetTemplateSummary where
  toQuery GetTemplateSummary' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("GetTemplateSummary" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "StackName" Core.=: stackName,
        "TemplateURL" Core.=: templateURL,
        "StackSetName" Core.=: stackSetName,
        "TemplateBody" Core.=: templateBody
      ]

-- | The output for the GetTemplateSummary action.
--
-- /See:/ 'newGetTemplateSummaryResponse' smart constructor.
data GetTemplateSummaryResponse = GetTemplateSummaryResponse'
  { -- | A list of all the template resource types that are defined in the
    -- template, such as @AWS::EC2::Instance@, @AWS::Dynamo::Table@, and
    -- @Custom::MyCustomInstance@.
    resourceTypes :: Core.Maybe [Core.Text],
    -- | The capabilities found within the template. If your template contains
    -- IAM resources, you must specify the CAPABILITY_IAM or
    -- CAPABILITY_NAMED_IAM value for this parameter when you use the
    -- CreateStack or UpdateStack actions with your template; otherwise, those
    -- actions return an InsufficientCapabilities error.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates>.
    capabilities :: Core.Maybe [Capability],
    -- | A list of resource identifier summaries that describe the target
    -- resources of an import operation and the properties you can provide
    -- during the import to identify the target resources. For example,
    -- @BucketName@ is a possible identifier property for an @AWS::S3::Bucket@
    -- resource.
    resourceIdentifierSummaries :: Core.Maybe [ResourceIdentifierSummary],
    -- | A list of the transforms that are declared in the template.
    declaredTransforms :: Core.Maybe [Core.Text],
    -- | The value that is defined for the @Metadata@ property of the template.
    metadata :: Core.Maybe Core.Text,
    -- | The AWS template format version, which identifies the capabilities of
    -- the template.
    version :: Core.Maybe Core.Text,
    -- | The value that is defined in the @Description@ property of the template.
    description :: Core.Maybe Core.Text,
    -- | A list of parameter declarations that describe various properties for
    -- each parameter.
    parameters :: Core.Maybe [ParameterDeclaration],
    -- | The list of resources that generated the values in the @Capabilities@
    -- response element.
    capabilitiesReason :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTemplateSummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceTypes', 'getTemplateSummaryResponse_resourceTypes' - A list of all the template resource types that are defined in the
-- template, such as @AWS::EC2::Instance@, @AWS::Dynamo::Table@, and
-- @Custom::MyCustomInstance@.
--
-- 'capabilities', 'getTemplateSummaryResponse_capabilities' - The capabilities found within the template. If your template contains
-- IAM resources, you must specify the CAPABILITY_IAM or
-- CAPABILITY_NAMED_IAM value for this parameter when you use the
-- CreateStack or UpdateStack actions with your template; otherwise, those
-- actions return an InsufficientCapabilities error.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates>.
--
-- 'resourceIdentifierSummaries', 'getTemplateSummaryResponse_resourceIdentifierSummaries' - A list of resource identifier summaries that describe the target
-- resources of an import operation and the properties you can provide
-- during the import to identify the target resources. For example,
-- @BucketName@ is a possible identifier property for an @AWS::S3::Bucket@
-- resource.
--
-- 'declaredTransforms', 'getTemplateSummaryResponse_declaredTransforms' - A list of the transforms that are declared in the template.
--
-- 'metadata', 'getTemplateSummaryResponse_metadata' - The value that is defined for the @Metadata@ property of the template.
--
-- 'version', 'getTemplateSummaryResponse_version' - The AWS template format version, which identifies the capabilities of
-- the template.
--
-- 'description', 'getTemplateSummaryResponse_description' - The value that is defined in the @Description@ property of the template.
--
-- 'parameters', 'getTemplateSummaryResponse_parameters' - A list of parameter declarations that describe various properties for
-- each parameter.
--
-- 'capabilitiesReason', 'getTemplateSummaryResponse_capabilitiesReason' - The list of resources that generated the values in the @Capabilities@
-- response element.
--
-- 'httpStatus', 'getTemplateSummaryResponse_httpStatus' - The response's http status code.
newGetTemplateSummaryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTemplateSummaryResponse
newGetTemplateSummaryResponse pHttpStatus_ =
  GetTemplateSummaryResponse'
    { resourceTypes =
        Core.Nothing,
      capabilities = Core.Nothing,
      resourceIdentifierSummaries = Core.Nothing,
      declaredTransforms = Core.Nothing,
      metadata = Core.Nothing,
      version = Core.Nothing,
      description = Core.Nothing,
      parameters = Core.Nothing,
      capabilitiesReason = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of all the template resource types that are defined in the
-- template, such as @AWS::EC2::Instance@, @AWS::Dynamo::Table@, and
-- @Custom::MyCustomInstance@.
getTemplateSummaryResponse_resourceTypes :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe [Core.Text])
getTemplateSummaryResponse_resourceTypes = Lens.lens (\GetTemplateSummaryResponse' {resourceTypes} -> resourceTypes) (\s@GetTemplateSummaryResponse' {} a -> s {resourceTypes = a} :: GetTemplateSummaryResponse) Core.. Lens.mapping Lens._Coerce

-- | The capabilities found within the template. If your template contains
-- IAM resources, you must specify the CAPABILITY_IAM or
-- CAPABILITY_NAMED_IAM value for this parameter when you use the
-- CreateStack or UpdateStack actions with your template; otherwise, those
-- actions return an InsufficientCapabilities error.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates>.
getTemplateSummaryResponse_capabilities :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe [Capability])
getTemplateSummaryResponse_capabilities = Lens.lens (\GetTemplateSummaryResponse' {capabilities} -> capabilities) (\s@GetTemplateSummaryResponse' {} a -> s {capabilities = a} :: GetTemplateSummaryResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of resource identifier summaries that describe the target
-- resources of an import operation and the properties you can provide
-- during the import to identify the target resources. For example,
-- @BucketName@ is a possible identifier property for an @AWS::S3::Bucket@
-- resource.
getTemplateSummaryResponse_resourceIdentifierSummaries :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe [ResourceIdentifierSummary])
getTemplateSummaryResponse_resourceIdentifierSummaries = Lens.lens (\GetTemplateSummaryResponse' {resourceIdentifierSummaries} -> resourceIdentifierSummaries) (\s@GetTemplateSummaryResponse' {} a -> s {resourceIdentifierSummaries = a} :: GetTemplateSummaryResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of the transforms that are declared in the template.
getTemplateSummaryResponse_declaredTransforms :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe [Core.Text])
getTemplateSummaryResponse_declaredTransforms = Lens.lens (\GetTemplateSummaryResponse' {declaredTransforms} -> declaredTransforms) (\s@GetTemplateSummaryResponse' {} a -> s {declaredTransforms = a} :: GetTemplateSummaryResponse) Core.. Lens.mapping Lens._Coerce

-- | The value that is defined for the @Metadata@ property of the template.
getTemplateSummaryResponse_metadata :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe Core.Text)
getTemplateSummaryResponse_metadata = Lens.lens (\GetTemplateSummaryResponse' {metadata} -> metadata) (\s@GetTemplateSummaryResponse' {} a -> s {metadata = a} :: GetTemplateSummaryResponse)

-- | The AWS template format version, which identifies the capabilities of
-- the template.
getTemplateSummaryResponse_version :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe Core.Text)
getTemplateSummaryResponse_version = Lens.lens (\GetTemplateSummaryResponse' {version} -> version) (\s@GetTemplateSummaryResponse' {} a -> s {version = a} :: GetTemplateSummaryResponse)

-- | The value that is defined in the @Description@ property of the template.
getTemplateSummaryResponse_description :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe Core.Text)
getTemplateSummaryResponse_description = Lens.lens (\GetTemplateSummaryResponse' {description} -> description) (\s@GetTemplateSummaryResponse' {} a -> s {description = a} :: GetTemplateSummaryResponse)

-- | A list of parameter declarations that describe various properties for
-- each parameter.
getTemplateSummaryResponse_parameters :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe [ParameterDeclaration])
getTemplateSummaryResponse_parameters = Lens.lens (\GetTemplateSummaryResponse' {parameters} -> parameters) (\s@GetTemplateSummaryResponse' {} a -> s {parameters = a} :: GetTemplateSummaryResponse) Core.. Lens.mapping Lens._Coerce

-- | The list of resources that generated the values in the @Capabilities@
-- response element.
getTemplateSummaryResponse_capabilitiesReason :: Lens.Lens' GetTemplateSummaryResponse (Core.Maybe Core.Text)
getTemplateSummaryResponse_capabilitiesReason = Lens.lens (\GetTemplateSummaryResponse' {capabilitiesReason} -> capabilitiesReason) (\s@GetTemplateSummaryResponse' {} a -> s {capabilitiesReason = a} :: GetTemplateSummaryResponse)

-- | The response's http status code.
getTemplateSummaryResponse_httpStatus :: Lens.Lens' GetTemplateSummaryResponse Core.Int
getTemplateSummaryResponse_httpStatus = Lens.lens (\GetTemplateSummaryResponse' {httpStatus} -> httpStatus) (\s@GetTemplateSummaryResponse' {} a -> s {httpStatus = a} :: GetTemplateSummaryResponse)

instance Core.NFData GetTemplateSummaryResponse
