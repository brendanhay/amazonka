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
import qualified Network.AWS.Prelude as Prelude
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
    stackName :: Prelude.Maybe Prelude.Text,
    -- | Location of file containing the template body. The URL must point to a
    -- template (max size: 460,800 bytes) that is located in an Amazon S3
    -- bucket or a Systems Manager document. For more information about
    -- templates, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
    templateURL :: Prelude.Maybe Prelude.Text,
    -- | The name or unique ID of the stack set from which the stack was created.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
    stackSetName :: Prelude.Maybe Prelude.Text,
    -- | Structure containing the template body with a minimum length of 1 byte
    -- and a maximum length of 51,200 bytes. For more information about
    -- templates, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
    templateBody :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { stackName = Prelude.Nothing,
      templateURL = Prelude.Nothing,
      stackSetName = Prelude.Nothing,
      templateBody = Prelude.Nothing
    }

-- | The name or the stack ID that is associated with the stack, which are
-- not always interchangeable. For running stacks, you can specify either
-- the stack\'s name or its unique stack ID. For deleted stack, you must
-- specify the unique stack ID.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
getTemplateSummary_stackName :: Lens.Lens' GetTemplateSummary (Prelude.Maybe Prelude.Text)
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
getTemplateSummary_templateURL :: Lens.Lens' GetTemplateSummary (Prelude.Maybe Prelude.Text)
getTemplateSummary_templateURL = Lens.lens (\GetTemplateSummary' {templateURL} -> templateURL) (\s@GetTemplateSummary' {} a -> s {templateURL = a} :: GetTemplateSummary)

-- | The name or unique ID of the stack set from which the stack was created.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
getTemplateSummary_stackSetName :: Lens.Lens' GetTemplateSummary (Prelude.Maybe Prelude.Text)
getTemplateSummary_stackSetName = Lens.lens (\GetTemplateSummary' {stackSetName} -> stackSetName) (\s@GetTemplateSummary' {} a -> s {stackSetName = a} :: GetTemplateSummary)

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. For more information about
-- templates, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
getTemplateSummary_templateBody :: Lens.Lens' GetTemplateSummary (Prelude.Maybe Prelude.Text)
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
            Prelude.<$> ( x Core..@? "ResourceTypes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> ( x Core..@? "Capabilities" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> ( x Core..@? "ResourceIdentifierSummaries"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> ( x Core..@? "DeclaredTransforms"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "Metadata")
            Prelude.<*> (x Core..@? "Version")
            Prelude.<*> (x Core..@? "Description")
            Prelude.<*> ( x Core..@? "Parameters" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "CapabilitiesReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTemplateSummary

instance Prelude.NFData GetTemplateSummary

instance Core.ToHeaders GetTemplateSummary where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetTemplateSummary where
  toPath = Prelude.const "/"

instance Core.ToQuery GetTemplateSummary where
  toQuery GetTemplateSummary' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("GetTemplateSummary" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
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
    resourceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The capabilities found within the template. If your template contains
    -- IAM resources, you must specify the CAPABILITY_IAM or
    -- CAPABILITY_NAMED_IAM value for this parameter when you use the
    -- CreateStack or UpdateStack actions with your template; otherwise, those
    -- actions return an InsufficientCapabilities error.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates>.
    capabilities :: Prelude.Maybe [Capability],
    -- | A list of resource identifier summaries that describe the target
    -- resources of an import operation and the properties you can provide
    -- during the import to identify the target resources. For example,
    -- @BucketName@ is a possible identifier property for an @AWS::S3::Bucket@
    -- resource.
    resourceIdentifierSummaries :: Prelude.Maybe [ResourceIdentifierSummary],
    -- | A list of the transforms that are declared in the template.
    declaredTransforms :: Prelude.Maybe [Prelude.Text],
    -- | The value that is defined for the @Metadata@ property of the template.
    metadata :: Prelude.Maybe Prelude.Text,
    -- | The AWS template format version, which identifies the capabilities of
    -- the template.
    version :: Prelude.Maybe Prelude.Text,
    -- | The value that is defined in the @Description@ property of the template.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of parameter declarations that describe various properties for
    -- each parameter.
    parameters :: Prelude.Maybe [ParameterDeclaration],
    -- | The list of resources that generated the values in the @Capabilities@
    -- response element.
    capabilitiesReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetTemplateSummaryResponse
newGetTemplateSummaryResponse pHttpStatus_ =
  GetTemplateSummaryResponse'
    { resourceTypes =
        Prelude.Nothing,
      capabilities = Prelude.Nothing,
      resourceIdentifierSummaries = Prelude.Nothing,
      declaredTransforms = Prelude.Nothing,
      metadata = Prelude.Nothing,
      version = Prelude.Nothing,
      description = Prelude.Nothing,
      parameters = Prelude.Nothing,
      capabilitiesReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of all the template resource types that are defined in the
-- template, such as @AWS::EC2::Instance@, @AWS::Dynamo::Table@, and
-- @Custom::MyCustomInstance@.
getTemplateSummaryResponse_resourceTypes :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe [Prelude.Text])
getTemplateSummaryResponse_resourceTypes = Lens.lens (\GetTemplateSummaryResponse' {resourceTypes} -> resourceTypes) (\s@GetTemplateSummaryResponse' {} a -> s {resourceTypes = a} :: GetTemplateSummaryResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The capabilities found within the template. If your template contains
-- IAM resources, you must specify the CAPABILITY_IAM or
-- CAPABILITY_NAMED_IAM value for this parameter when you use the
-- CreateStack or UpdateStack actions with your template; otherwise, those
-- actions return an InsufficientCapabilities error.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates>.
getTemplateSummaryResponse_capabilities :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe [Capability])
getTemplateSummaryResponse_capabilities = Lens.lens (\GetTemplateSummaryResponse' {capabilities} -> capabilities) (\s@GetTemplateSummaryResponse' {} a -> s {capabilities = a} :: GetTemplateSummaryResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of resource identifier summaries that describe the target
-- resources of an import operation and the properties you can provide
-- during the import to identify the target resources. For example,
-- @BucketName@ is a possible identifier property for an @AWS::S3::Bucket@
-- resource.
getTemplateSummaryResponse_resourceIdentifierSummaries :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe [ResourceIdentifierSummary])
getTemplateSummaryResponse_resourceIdentifierSummaries = Lens.lens (\GetTemplateSummaryResponse' {resourceIdentifierSummaries} -> resourceIdentifierSummaries) (\s@GetTemplateSummaryResponse' {} a -> s {resourceIdentifierSummaries = a} :: GetTemplateSummaryResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of the transforms that are declared in the template.
getTemplateSummaryResponse_declaredTransforms :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe [Prelude.Text])
getTemplateSummaryResponse_declaredTransforms = Lens.lens (\GetTemplateSummaryResponse' {declaredTransforms} -> declaredTransforms) (\s@GetTemplateSummaryResponse' {} a -> s {declaredTransforms = a} :: GetTemplateSummaryResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The value that is defined for the @Metadata@ property of the template.
getTemplateSummaryResponse_metadata :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe Prelude.Text)
getTemplateSummaryResponse_metadata = Lens.lens (\GetTemplateSummaryResponse' {metadata} -> metadata) (\s@GetTemplateSummaryResponse' {} a -> s {metadata = a} :: GetTemplateSummaryResponse)

-- | The AWS template format version, which identifies the capabilities of
-- the template.
getTemplateSummaryResponse_version :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe Prelude.Text)
getTemplateSummaryResponse_version = Lens.lens (\GetTemplateSummaryResponse' {version} -> version) (\s@GetTemplateSummaryResponse' {} a -> s {version = a} :: GetTemplateSummaryResponse)

-- | The value that is defined in the @Description@ property of the template.
getTemplateSummaryResponse_description :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe Prelude.Text)
getTemplateSummaryResponse_description = Lens.lens (\GetTemplateSummaryResponse' {description} -> description) (\s@GetTemplateSummaryResponse' {} a -> s {description = a} :: GetTemplateSummaryResponse)

-- | A list of parameter declarations that describe various properties for
-- each parameter.
getTemplateSummaryResponse_parameters :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe [ParameterDeclaration])
getTemplateSummaryResponse_parameters = Lens.lens (\GetTemplateSummaryResponse' {parameters} -> parameters) (\s@GetTemplateSummaryResponse' {} a -> s {parameters = a} :: GetTemplateSummaryResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The list of resources that generated the values in the @Capabilities@
-- response element.
getTemplateSummaryResponse_capabilitiesReason :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe Prelude.Text)
getTemplateSummaryResponse_capabilitiesReason = Lens.lens (\GetTemplateSummaryResponse' {capabilitiesReason} -> capabilitiesReason) (\s@GetTemplateSummaryResponse' {} a -> s {capabilitiesReason = a} :: GetTemplateSummaryResponse)

-- | The response's http status code.
getTemplateSummaryResponse_httpStatus :: Lens.Lens' GetTemplateSummaryResponse Prelude.Int
getTemplateSummaryResponse_httpStatus = Lens.lens (\GetTemplateSummaryResponse' {httpStatus} -> httpStatus) (\s@GetTemplateSummaryResponse' {} a -> s {httpStatus = a} :: GetTemplateSummaryResponse)

instance Prelude.NFData GetTemplateSummaryResponse
