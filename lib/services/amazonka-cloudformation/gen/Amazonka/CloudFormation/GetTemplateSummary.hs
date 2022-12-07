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
-- Module      : Amazonka.CloudFormation.GetTemplateSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- template doesn\'t exist, a @ValidationError@ is returned.
module Amazonka.CloudFormation.GetTemplateSummary
  ( -- * Creating a Request
    GetTemplateSummary (..),
    newGetTemplateSummary,

    -- * Request Lenses
    getTemplateSummary_stackSetName,
    getTemplateSummary_callAs,
    getTemplateSummary_templateBody,
    getTemplateSummary_stackName,
    getTemplateSummary_templateURL,

    -- * Destructuring the Response
    GetTemplateSummaryResponse (..),
    newGetTemplateSummaryResponse,

    -- * Response Lenses
    getTemplateSummaryResponse_capabilitiesReason,
    getTemplateSummaryResponse_metadata,
    getTemplateSummaryResponse_resourceTypes,
    getTemplateSummaryResponse_resourceIdentifierSummaries,
    getTemplateSummaryResponse_description,
    getTemplateSummaryResponse_capabilities,
    getTemplateSummaryResponse_declaredTransforms,
    getTemplateSummaryResponse_version,
    getTemplateSummaryResponse_parameters,
    getTemplateSummaryResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the GetTemplateSummary action.
--
-- /See:/ 'newGetTemplateSummary' smart constructor.
data GetTemplateSummary = GetTemplateSummary'
  { -- | The name or unique ID of the stack set from which the stack was created.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
    stackSetName :: Prelude.Maybe Prelude.Text,
    -- | [Service-managed permissions] Specifies whether you are acting as an
    -- account administrator in the organization\'s management account or as a
    -- delegated administrator in a member account.
    --
    -- By default, @SELF@ is specified. Use @SELF@ for stack sets with
    -- self-managed permissions.
    --
    -- -   If you are signed in to the management account, specify @SELF@.
    --
    -- -   If you are signed in to a delegated administrator account, specify
    --     @DELEGATED_ADMIN@.
    --
    --     Your Amazon Web Services account must be registered as a delegated
    --     administrator in the management account. For more information, see
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
    --     in the /CloudFormation User Guide/.
    callAs :: Prelude.Maybe CallAs,
    -- | Structure containing the template body with a minimum length of 1 byte
    -- and a maximum length of 51,200 bytes. For more information about
    -- templates, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template anatomy>
    -- in the CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | The name or the stack ID that\'s associated with the stack, which
    -- aren\'t always interchangeable. For running stacks, you can specify
    -- either the stack\'s name or its unique stack ID. For deleted stack, you
    -- must specify the unique stack ID.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
    stackName :: Prelude.Maybe Prelude.Text,
    -- | Location of file containing the template body. The URL must point to a
    -- template (max size: 460,800 bytes) that\'s located in an Amazon S3
    -- bucket or a Systems Manager document. For more information about
    -- templates, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template anatomy>
    -- in the CloudFormation User Guide.
    --
    -- Conditional: You must specify only one of the following parameters:
    -- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
    templateURL :: Prelude.Maybe Prelude.Text
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
-- 'stackSetName', 'getTemplateSummary_stackSetName' - The name or unique ID of the stack set from which the stack was created.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
--
-- 'callAs', 'getTemplateSummary_callAs' - [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   If you are signed in to a delegated administrator account, specify
--     @DELEGATED_ADMIN@.
--
--     Your Amazon Web Services account must be registered as a delegated
--     administrator in the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /CloudFormation User Guide/.
--
-- 'templateBody', 'getTemplateSummary_templateBody' - Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. For more information about
-- templates, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template anatomy>
-- in the CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
--
-- 'stackName', 'getTemplateSummary_stackName' - The name or the stack ID that\'s associated with the stack, which
-- aren\'t always interchangeable. For running stacks, you can specify
-- either the stack\'s name or its unique stack ID. For deleted stack, you
-- must specify the unique stack ID.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
--
-- 'templateURL', 'getTemplateSummary_templateURL' - Location of file containing the template body. The URL must point to a
-- template (max size: 460,800 bytes) that\'s located in an Amazon S3
-- bucket or a Systems Manager document. For more information about
-- templates, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template anatomy>
-- in the CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
newGetTemplateSummary ::
  GetTemplateSummary
newGetTemplateSummary =
  GetTemplateSummary'
    { stackSetName = Prelude.Nothing,
      callAs = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      stackName = Prelude.Nothing,
      templateURL = Prelude.Nothing
    }

-- | The name or unique ID of the stack set from which the stack was created.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
getTemplateSummary_stackSetName :: Lens.Lens' GetTemplateSummary (Prelude.Maybe Prelude.Text)
getTemplateSummary_stackSetName = Lens.lens (\GetTemplateSummary' {stackSetName} -> stackSetName) (\s@GetTemplateSummary' {} a -> s {stackSetName = a} :: GetTemplateSummary)

-- | [Service-managed permissions] Specifies whether you are acting as an
-- account administrator in the organization\'s management account or as a
-- delegated administrator in a member account.
--
-- By default, @SELF@ is specified. Use @SELF@ for stack sets with
-- self-managed permissions.
--
-- -   If you are signed in to the management account, specify @SELF@.
--
-- -   If you are signed in to a delegated administrator account, specify
--     @DELEGATED_ADMIN@.
--
--     Your Amazon Web Services account must be registered as a delegated
--     administrator in the management account. For more information, see
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-orgs-delegated-admin.html Register a delegated administrator>
--     in the /CloudFormation User Guide/.
getTemplateSummary_callAs :: Lens.Lens' GetTemplateSummary (Prelude.Maybe CallAs)
getTemplateSummary_callAs = Lens.lens (\GetTemplateSummary' {callAs} -> callAs) (\s@GetTemplateSummary' {} a -> s {callAs = a} :: GetTemplateSummary)

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. For more information about
-- templates, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template anatomy>
-- in the CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
getTemplateSummary_templateBody :: Lens.Lens' GetTemplateSummary (Prelude.Maybe Prelude.Text)
getTemplateSummary_templateBody = Lens.lens (\GetTemplateSummary' {templateBody} -> templateBody) (\s@GetTemplateSummary' {} a -> s {templateBody = a} :: GetTemplateSummary)

-- | The name or the stack ID that\'s associated with the stack, which
-- aren\'t always interchangeable. For running stacks, you can specify
-- either the stack\'s name or its unique stack ID. For deleted stack, you
-- must specify the unique stack ID.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
getTemplateSummary_stackName :: Lens.Lens' GetTemplateSummary (Prelude.Maybe Prelude.Text)
getTemplateSummary_stackName = Lens.lens (\GetTemplateSummary' {stackName} -> stackName) (\s@GetTemplateSummary' {} a -> s {stackName = a} :: GetTemplateSummary)

-- | Location of file containing the template body. The URL must point to a
-- template (max size: 460,800 bytes) that\'s located in an Amazon S3
-- bucket or a Systems Manager document. For more information about
-- templates, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template anatomy>
-- in the CloudFormation User Guide.
--
-- Conditional: You must specify only one of the following parameters:
-- @StackName@, @StackSetName@, @TemplateBody@, or @TemplateURL@.
getTemplateSummary_templateURL :: Lens.Lens' GetTemplateSummary (Prelude.Maybe Prelude.Text)
getTemplateSummary_templateURL = Lens.lens (\GetTemplateSummary' {templateURL} -> templateURL) (\s@GetTemplateSummary' {} a -> s {templateURL = a} :: GetTemplateSummary)

instance Core.AWSRequest GetTemplateSummary where
  type
    AWSResponse GetTemplateSummary =
      GetTemplateSummaryResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetTemplateSummaryResult"
      ( \s h x ->
          GetTemplateSummaryResponse'
            Prelude.<$> (x Data..@? "CapabilitiesReason")
            Prelude.<*> (x Data..@? "Metadata")
            Prelude.<*> ( x Data..@? "ResourceTypes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> ( x Data..@? "ResourceIdentifierSummaries"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "Description")
            Prelude.<*> ( x Data..@? "Capabilities" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> ( x Data..@? "DeclaredTransforms"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "Version")
            Prelude.<*> ( x Data..@? "Parameters" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTemplateSummary where
  hashWithSalt _salt GetTemplateSummary' {..} =
    _salt `Prelude.hashWithSalt` stackSetName
      `Prelude.hashWithSalt` callAs
      `Prelude.hashWithSalt` templateBody
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` templateURL

instance Prelude.NFData GetTemplateSummary where
  rnf GetTemplateSummary' {..} =
    Prelude.rnf stackSetName
      `Prelude.seq` Prelude.rnf callAs
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf templateURL

instance Data.ToHeaders GetTemplateSummary where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetTemplateSummary where
  toPath = Prelude.const "/"

instance Data.ToQuery GetTemplateSummary where
  toQuery GetTemplateSummary' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetTemplateSummary" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "StackSetName" Data.=: stackSetName,
        "CallAs" Data.=: callAs,
        "TemplateBody" Data.=: templateBody,
        "StackName" Data.=: stackName,
        "TemplateURL" Data.=: templateURL
      ]

-- | The output for the GetTemplateSummary action.
--
-- /See:/ 'newGetTemplateSummaryResponse' smart constructor.
data GetTemplateSummaryResponse = GetTemplateSummaryResponse'
  { -- | The list of resources that generated the values in the @Capabilities@
    -- response element.
    capabilitiesReason :: Prelude.Maybe Prelude.Text,
    -- | The value that\'s defined for the @Metadata@ property of the template.
    metadata :: Prelude.Maybe Prelude.Text,
    -- | A list of all the template resource types that are defined in the
    -- template, such as @AWS::EC2::Instance@, @AWS::Dynamo::Table@, and
    -- @Custom::MyCustomInstance@.
    resourceTypes :: Prelude.Maybe [Prelude.Text],
    -- | A list of resource identifier summaries that describe the target
    -- resources of an import operation and the properties you can provide
    -- during the import to identify the target resources. For example,
    -- @BucketName@ is a possible identifier property for an @AWS::S3::Bucket@
    -- resource.
    resourceIdentifierSummaries :: Prelude.Maybe [ResourceIdentifierSummary],
    -- | The value that\'s defined in the @Description@ property of the template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The capabilities found within the template. If your template contains
    -- IAM resources, you must specify the @CAPABILITY_IAM@ or
    -- @CAPABILITY_NAMED_IAM@ value for this parameter when you use the
    -- CreateStack or UpdateStack actions with your template; otherwise, those
    -- actions return an @InsufficientCapabilities@ error.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in CloudFormation Templates>.
    capabilities :: Prelude.Maybe [Capability],
    -- | A list of the transforms that are declared in the template.
    declaredTransforms :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Web Services template format version, which identifies the
    -- capabilities of the template.
    version :: Prelude.Maybe Prelude.Text,
    -- | A list of parameter declarations that describe various properties for
    -- each parameter.
    parameters :: Prelude.Maybe [ParameterDeclaration],
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
-- 'capabilitiesReason', 'getTemplateSummaryResponse_capabilitiesReason' - The list of resources that generated the values in the @Capabilities@
-- response element.
--
-- 'metadata', 'getTemplateSummaryResponse_metadata' - The value that\'s defined for the @Metadata@ property of the template.
--
-- 'resourceTypes', 'getTemplateSummaryResponse_resourceTypes' - A list of all the template resource types that are defined in the
-- template, such as @AWS::EC2::Instance@, @AWS::Dynamo::Table@, and
-- @Custom::MyCustomInstance@.
--
-- 'resourceIdentifierSummaries', 'getTemplateSummaryResponse_resourceIdentifierSummaries' - A list of resource identifier summaries that describe the target
-- resources of an import operation and the properties you can provide
-- during the import to identify the target resources. For example,
-- @BucketName@ is a possible identifier property for an @AWS::S3::Bucket@
-- resource.
--
-- 'description', 'getTemplateSummaryResponse_description' - The value that\'s defined in the @Description@ property of the template.
--
-- 'capabilities', 'getTemplateSummaryResponse_capabilities' - The capabilities found within the template. If your template contains
-- IAM resources, you must specify the @CAPABILITY_IAM@ or
-- @CAPABILITY_NAMED_IAM@ value for this parameter when you use the
-- CreateStack or UpdateStack actions with your template; otherwise, those
-- actions return an @InsufficientCapabilities@ error.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in CloudFormation Templates>.
--
-- 'declaredTransforms', 'getTemplateSummaryResponse_declaredTransforms' - A list of the transforms that are declared in the template.
--
-- 'version', 'getTemplateSummaryResponse_version' - The Amazon Web Services template format version, which identifies the
-- capabilities of the template.
--
-- 'parameters', 'getTemplateSummaryResponse_parameters' - A list of parameter declarations that describe various properties for
-- each parameter.
--
-- 'httpStatus', 'getTemplateSummaryResponse_httpStatus' - The response's http status code.
newGetTemplateSummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTemplateSummaryResponse
newGetTemplateSummaryResponse pHttpStatus_ =
  GetTemplateSummaryResponse'
    { capabilitiesReason =
        Prelude.Nothing,
      metadata = Prelude.Nothing,
      resourceTypes = Prelude.Nothing,
      resourceIdentifierSummaries = Prelude.Nothing,
      description = Prelude.Nothing,
      capabilities = Prelude.Nothing,
      declaredTransforms = Prelude.Nothing,
      version = Prelude.Nothing,
      parameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of resources that generated the values in the @Capabilities@
-- response element.
getTemplateSummaryResponse_capabilitiesReason :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe Prelude.Text)
getTemplateSummaryResponse_capabilitiesReason = Lens.lens (\GetTemplateSummaryResponse' {capabilitiesReason} -> capabilitiesReason) (\s@GetTemplateSummaryResponse' {} a -> s {capabilitiesReason = a} :: GetTemplateSummaryResponse)

-- | The value that\'s defined for the @Metadata@ property of the template.
getTemplateSummaryResponse_metadata :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe Prelude.Text)
getTemplateSummaryResponse_metadata = Lens.lens (\GetTemplateSummaryResponse' {metadata} -> metadata) (\s@GetTemplateSummaryResponse' {} a -> s {metadata = a} :: GetTemplateSummaryResponse)

-- | A list of all the template resource types that are defined in the
-- template, such as @AWS::EC2::Instance@, @AWS::Dynamo::Table@, and
-- @Custom::MyCustomInstance@.
getTemplateSummaryResponse_resourceTypes :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe [Prelude.Text])
getTemplateSummaryResponse_resourceTypes = Lens.lens (\GetTemplateSummaryResponse' {resourceTypes} -> resourceTypes) (\s@GetTemplateSummaryResponse' {} a -> s {resourceTypes = a} :: GetTemplateSummaryResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of resource identifier summaries that describe the target
-- resources of an import operation and the properties you can provide
-- during the import to identify the target resources. For example,
-- @BucketName@ is a possible identifier property for an @AWS::S3::Bucket@
-- resource.
getTemplateSummaryResponse_resourceIdentifierSummaries :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe [ResourceIdentifierSummary])
getTemplateSummaryResponse_resourceIdentifierSummaries = Lens.lens (\GetTemplateSummaryResponse' {resourceIdentifierSummaries} -> resourceIdentifierSummaries) (\s@GetTemplateSummaryResponse' {} a -> s {resourceIdentifierSummaries = a} :: GetTemplateSummaryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The value that\'s defined in the @Description@ property of the template.
getTemplateSummaryResponse_description :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe Prelude.Text)
getTemplateSummaryResponse_description = Lens.lens (\GetTemplateSummaryResponse' {description} -> description) (\s@GetTemplateSummaryResponse' {} a -> s {description = a} :: GetTemplateSummaryResponse)

-- | The capabilities found within the template. If your template contains
-- IAM resources, you must specify the @CAPABILITY_IAM@ or
-- @CAPABILITY_NAMED_IAM@ value for this parameter when you use the
-- CreateStack or UpdateStack actions with your template; otherwise, those
-- actions return an @InsufficientCapabilities@ error.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in CloudFormation Templates>.
getTemplateSummaryResponse_capabilities :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe [Capability])
getTemplateSummaryResponse_capabilities = Lens.lens (\GetTemplateSummaryResponse' {capabilities} -> capabilities) (\s@GetTemplateSummaryResponse' {} a -> s {capabilities = a} :: GetTemplateSummaryResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the transforms that are declared in the template.
getTemplateSummaryResponse_declaredTransforms :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe [Prelude.Text])
getTemplateSummaryResponse_declaredTransforms = Lens.lens (\GetTemplateSummaryResponse' {declaredTransforms} -> declaredTransforms) (\s@GetTemplateSummaryResponse' {} a -> s {declaredTransforms = a} :: GetTemplateSummaryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services template format version, which identifies the
-- capabilities of the template.
getTemplateSummaryResponse_version :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe Prelude.Text)
getTemplateSummaryResponse_version = Lens.lens (\GetTemplateSummaryResponse' {version} -> version) (\s@GetTemplateSummaryResponse' {} a -> s {version = a} :: GetTemplateSummaryResponse)

-- | A list of parameter declarations that describe various properties for
-- each parameter.
getTemplateSummaryResponse_parameters :: Lens.Lens' GetTemplateSummaryResponse (Prelude.Maybe [ParameterDeclaration])
getTemplateSummaryResponse_parameters = Lens.lens (\GetTemplateSummaryResponse' {parameters} -> parameters) (\s@GetTemplateSummaryResponse' {} a -> s {parameters = a} :: GetTemplateSummaryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getTemplateSummaryResponse_httpStatus :: Lens.Lens' GetTemplateSummaryResponse Prelude.Int
getTemplateSummaryResponse_httpStatus = Lens.lens (\GetTemplateSummaryResponse' {httpStatus} -> httpStatus) (\s@GetTemplateSummaryResponse' {} a -> s {httpStatus = a} :: GetTemplateSummaryResponse)

instance Prelude.NFData GetTemplateSummaryResponse where
  rnf GetTemplateSummaryResponse' {..} =
    Prelude.rnf capabilitiesReason
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf resourceTypes
      `Prelude.seq` Prelude.rnf resourceIdentifierSummaries
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf declaredTransforms
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf httpStatus
