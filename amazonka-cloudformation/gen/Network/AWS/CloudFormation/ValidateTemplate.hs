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
-- Module      : Network.AWS.CloudFormation.ValidateTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates a specified template. AWS CloudFormation first checks if the
-- template is valid JSON. If it isn\'t, AWS CloudFormation checks if the
-- template is valid YAML. If both these checks fail, AWS CloudFormation
-- returns a template validation error.
module Network.AWS.CloudFormation.ValidateTemplate
  ( -- * Creating a Request
    ValidateTemplate (..),
    newValidateTemplate,

    -- * Request Lenses
    validateTemplate_templateURL,
    validateTemplate_templateBody,

    -- * Destructuring the Response
    ValidateTemplateResponse (..),
    newValidateTemplateResponse,

    -- * Response Lenses
    validateTemplateResponse_capabilities,
    validateTemplateResponse_declaredTransforms,
    validateTemplateResponse_description,
    validateTemplateResponse_parameters,
    validateTemplateResponse_capabilitiesReason,
    validateTemplateResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for ValidateTemplate action.
--
-- /See:/ 'newValidateTemplate' smart constructor.
data ValidateTemplate = ValidateTemplate'
  { -- | Location of file containing the template body. The URL must point to a
    -- template (max size: 460,800 bytes) that is located in an Amazon S3
    -- bucket or a Systems Manager document. For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must pass @TemplateURL@ or @TemplateBody@. If both are
    -- passed, only @TemplateBody@ is used.
    templateURL :: Prelude.Maybe Prelude.Text,
    -- | Structure containing the template body with a minimum length of 1 byte
    -- and a maximum length of 51,200 bytes. For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must pass @TemplateURL@ or @TemplateBody@. If both are
    -- passed, only @TemplateBody@ is used.
    templateBody :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateURL', 'validateTemplate_templateURL' - Location of file containing the template body. The URL must point to a
-- template (max size: 460,800 bytes) that is located in an Amazon S3
-- bucket or a Systems Manager document. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@. If both are
-- passed, only @TemplateBody@ is used.
--
-- 'templateBody', 'validateTemplate_templateBody' - Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@. If both are
-- passed, only @TemplateBody@ is used.
newValidateTemplate ::
  ValidateTemplate
newValidateTemplate =
  ValidateTemplate'
    { templateURL = Prelude.Nothing,
      templateBody = Prelude.Nothing
    }

-- | Location of file containing the template body. The URL must point to a
-- template (max size: 460,800 bytes) that is located in an Amazon S3
-- bucket or a Systems Manager document. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@. If both are
-- passed, only @TemplateBody@ is used.
validateTemplate_templateURL :: Lens.Lens' ValidateTemplate (Prelude.Maybe Prelude.Text)
validateTemplate_templateURL = Lens.lens (\ValidateTemplate' {templateURL} -> templateURL) (\s@ValidateTemplate' {} a -> s {templateURL = a} :: ValidateTemplate)

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@. If both are
-- passed, only @TemplateBody@ is used.
validateTemplate_templateBody :: Lens.Lens' ValidateTemplate (Prelude.Maybe Prelude.Text)
validateTemplate_templateBody = Lens.lens (\ValidateTemplate' {templateBody} -> templateBody) (\s@ValidateTemplate' {} a -> s {templateBody = a} :: ValidateTemplate)

instance Core.AWSRequest ValidateTemplate where
  type
    AWSResponse ValidateTemplate =
      ValidateTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ValidateTemplateResult"
      ( \s h x ->
          ValidateTemplateResponse'
            Prelude.<$> ( x Core..@? "Capabilities" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> ( x Core..@? "DeclaredTransforms"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "Description")
            Prelude.<*> ( x Core..@? "Parameters" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "CapabilitiesReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ValidateTemplate

instance Prelude.NFData ValidateTemplate

instance Core.ToHeaders ValidateTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ValidateTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery ValidateTemplate where
  toQuery ValidateTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ValidateTemplate" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "TemplateURL" Core.=: templateURL,
        "TemplateBody" Core.=: templateBody
      ]

-- | The output for ValidateTemplate action.
--
-- /See:/ 'newValidateTemplateResponse' smart constructor.
data ValidateTemplateResponse = ValidateTemplateResponse'
  { -- | The capabilities found within the template. If your template contains
    -- IAM resources, you must specify the CAPABILITY_IAM or
    -- CAPABILITY_NAMED_IAM value for this parameter when you use the
    -- CreateStack or UpdateStack actions with your template; otherwise, those
    -- actions return an InsufficientCapabilities error.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates>.
    capabilities :: Prelude.Maybe [Capability],
    -- | A list of the transforms that are declared in the template.
    declaredTransforms :: Prelude.Maybe [Prelude.Text],
    -- | The description found within the template.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of @TemplateParameter@ structures.
    parameters :: Prelude.Maybe [TemplateParameter],
    -- | The list of resources that generated the values in the @Capabilities@
    -- response element.
    capabilitiesReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capabilities', 'validateTemplateResponse_capabilities' - The capabilities found within the template. If your template contains
-- IAM resources, you must specify the CAPABILITY_IAM or
-- CAPABILITY_NAMED_IAM value for this parameter when you use the
-- CreateStack or UpdateStack actions with your template; otherwise, those
-- actions return an InsufficientCapabilities error.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates>.
--
-- 'declaredTransforms', 'validateTemplateResponse_declaredTransforms' - A list of the transforms that are declared in the template.
--
-- 'description', 'validateTemplateResponse_description' - The description found within the template.
--
-- 'parameters', 'validateTemplateResponse_parameters' - A list of @TemplateParameter@ structures.
--
-- 'capabilitiesReason', 'validateTemplateResponse_capabilitiesReason' - The list of resources that generated the values in the @Capabilities@
-- response element.
--
-- 'httpStatus', 'validateTemplateResponse_httpStatus' - The response's http status code.
newValidateTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ValidateTemplateResponse
newValidateTemplateResponse pHttpStatus_ =
  ValidateTemplateResponse'
    { capabilities =
        Prelude.Nothing,
      declaredTransforms = Prelude.Nothing,
      description = Prelude.Nothing,
      parameters = Prelude.Nothing,
      capabilitiesReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The capabilities found within the template. If your template contains
-- IAM resources, you must specify the CAPABILITY_IAM or
-- CAPABILITY_NAMED_IAM value for this parameter when you use the
-- CreateStack or UpdateStack actions with your template; otherwise, those
-- actions return an InsufficientCapabilities error.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-iam-template.html#capabilities Acknowledging IAM Resources in AWS CloudFormation Templates>.
validateTemplateResponse_capabilities :: Lens.Lens' ValidateTemplateResponse (Prelude.Maybe [Capability])
validateTemplateResponse_capabilities = Lens.lens (\ValidateTemplateResponse' {capabilities} -> capabilities) (\s@ValidateTemplateResponse' {} a -> s {capabilities = a} :: ValidateTemplateResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of the transforms that are declared in the template.
validateTemplateResponse_declaredTransforms :: Lens.Lens' ValidateTemplateResponse (Prelude.Maybe [Prelude.Text])
validateTemplateResponse_declaredTransforms = Lens.lens (\ValidateTemplateResponse' {declaredTransforms} -> declaredTransforms) (\s@ValidateTemplateResponse' {} a -> s {declaredTransforms = a} :: ValidateTemplateResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The description found within the template.
validateTemplateResponse_description :: Lens.Lens' ValidateTemplateResponse (Prelude.Maybe Prelude.Text)
validateTemplateResponse_description = Lens.lens (\ValidateTemplateResponse' {description} -> description) (\s@ValidateTemplateResponse' {} a -> s {description = a} :: ValidateTemplateResponse)

-- | A list of @TemplateParameter@ structures.
validateTemplateResponse_parameters :: Lens.Lens' ValidateTemplateResponse (Prelude.Maybe [TemplateParameter])
validateTemplateResponse_parameters = Lens.lens (\ValidateTemplateResponse' {parameters} -> parameters) (\s@ValidateTemplateResponse' {} a -> s {parameters = a} :: ValidateTemplateResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The list of resources that generated the values in the @Capabilities@
-- response element.
validateTemplateResponse_capabilitiesReason :: Lens.Lens' ValidateTemplateResponse (Prelude.Maybe Prelude.Text)
validateTemplateResponse_capabilitiesReason = Lens.lens (\ValidateTemplateResponse' {capabilitiesReason} -> capabilitiesReason) (\s@ValidateTemplateResponse' {} a -> s {capabilitiesReason = a} :: ValidateTemplateResponse)

-- | The response's http status code.
validateTemplateResponse_httpStatus :: Lens.Lens' ValidateTemplateResponse Prelude.Int
validateTemplateResponse_httpStatus = Lens.lens (\ValidateTemplateResponse' {httpStatus} -> httpStatus) (\s@ValidateTemplateResponse' {} a -> s {httpStatus = a} :: ValidateTemplateResponse)

instance Prelude.NFData ValidateTemplateResponse
