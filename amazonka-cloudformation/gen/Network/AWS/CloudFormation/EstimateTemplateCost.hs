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
-- Module      : Network.AWS.CloudFormation.EstimateTemplateCost
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the estimated monthly cost of a template. The return value is an
-- AWS Simple Monthly Calculator URL with a query string that describes the
-- resources required to run the template.
module Network.AWS.CloudFormation.EstimateTemplateCost
  ( -- * Creating a Request
    EstimateTemplateCost (..),
    newEstimateTemplateCost,

    -- * Request Lenses
    estimateTemplateCost_templateURL,
    estimateTemplateCost_templateBody,
    estimateTemplateCost_parameters,

    -- * Destructuring the Response
    EstimateTemplateCostResponse (..),
    newEstimateTemplateCostResponse,

    -- * Response Lenses
    estimateTemplateCostResponse_url,
    estimateTemplateCostResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for an EstimateTemplateCost action.
--
-- /See:/ 'newEstimateTemplateCost' smart constructor.
data EstimateTemplateCost = EstimateTemplateCost'
  { -- | Location of file containing the template body. The URL must point to a
    -- template that is located in an Amazon S3 bucket or a Systems Manager
    -- document. For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the AWS CloudFormation User Guide.
    --
    -- Conditional: You must pass @TemplateURL@ or @TemplateBody@. If both are
    -- passed, only @TemplateBody@ is used.
    templateURL :: Core.Maybe Core.Text,
    -- | Structure containing the template body with a minimum length of 1 byte
    -- and a maximum length of 51,200 bytes. (For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the AWS CloudFormation User Guide.)
    --
    -- Conditional: You must pass @TemplateBody@ or @TemplateURL@. If both are
    -- passed, only @TemplateBody@ is used.
    templateBody :: Core.Maybe Core.Text,
    -- | A list of @Parameter@ structures that specify input parameters.
    parameters :: Core.Maybe [Parameter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EstimateTemplateCost' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateURL', 'estimateTemplateCost_templateURL' - Location of file containing the template body. The URL must point to a
-- template that is located in an Amazon S3 bucket or a Systems Manager
-- document. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@. If both are
-- passed, only @TemplateBody@ is used.
--
-- 'templateBody', 'estimateTemplateCost_templateBody' - Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.)
--
-- Conditional: You must pass @TemplateBody@ or @TemplateURL@. If both are
-- passed, only @TemplateBody@ is used.
--
-- 'parameters', 'estimateTemplateCost_parameters' - A list of @Parameter@ structures that specify input parameters.
newEstimateTemplateCost ::
  EstimateTemplateCost
newEstimateTemplateCost =
  EstimateTemplateCost'
    { templateURL = Core.Nothing,
      templateBody = Core.Nothing,
      parameters = Core.Nothing
    }

-- | Location of file containing the template body. The URL must point to a
-- template that is located in an Amazon S3 bucket or a Systems Manager
-- document. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@. If both are
-- passed, only @TemplateBody@ is used.
estimateTemplateCost_templateURL :: Lens.Lens' EstimateTemplateCost (Core.Maybe Core.Text)
estimateTemplateCost_templateURL = Lens.lens (\EstimateTemplateCost' {templateURL} -> templateURL) (\s@EstimateTemplateCost' {} a -> s {templateURL = a} :: EstimateTemplateCost)

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the AWS CloudFormation User Guide.)
--
-- Conditional: You must pass @TemplateBody@ or @TemplateURL@. If both are
-- passed, only @TemplateBody@ is used.
estimateTemplateCost_templateBody :: Lens.Lens' EstimateTemplateCost (Core.Maybe Core.Text)
estimateTemplateCost_templateBody = Lens.lens (\EstimateTemplateCost' {templateBody} -> templateBody) (\s@EstimateTemplateCost' {} a -> s {templateBody = a} :: EstimateTemplateCost)

-- | A list of @Parameter@ structures that specify input parameters.
estimateTemplateCost_parameters :: Lens.Lens' EstimateTemplateCost (Core.Maybe [Parameter])
estimateTemplateCost_parameters = Lens.lens (\EstimateTemplateCost' {parameters} -> parameters) (\s@EstimateTemplateCost' {} a -> s {parameters = a} :: EstimateTemplateCost) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest EstimateTemplateCost where
  type
    AWSResponse EstimateTemplateCost =
      EstimateTemplateCostResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "EstimateTemplateCostResult"
      ( \s h x ->
          EstimateTemplateCostResponse'
            Core.<$> (x Core..@? "Url")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable EstimateTemplateCost

instance Core.NFData EstimateTemplateCost

instance Core.ToHeaders EstimateTemplateCost where
  toHeaders = Core.const Core.mempty

instance Core.ToPath EstimateTemplateCost where
  toPath = Core.const "/"

instance Core.ToQuery EstimateTemplateCost where
  toQuery EstimateTemplateCost' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("EstimateTemplateCost" :: Core.ByteString),
        "Version" Core.=: ("2010-05-15" :: Core.ByteString),
        "TemplateURL" Core.=: templateURL,
        "TemplateBody" Core.=: templateBody,
        "Parameters"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> parameters)
      ]

-- | The output for a EstimateTemplateCost action.
--
-- /See:/ 'newEstimateTemplateCostResponse' smart constructor.
data EstimateTemplateCostResponse = EstimateTemplateCostResponse'
  { -- | An AWS Simple Monthly Calculator URL with a query string that describes
    -- the resources required to run the template.
    url :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EstimateTemplateCostResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'estimateTemplateCostResponse_url' - An AWS Simple Monthly Calculator URL with a query string that describes
-- the resources required to run the template.
--
-- 'httpStatus', 'estimateTemplateCostResponse_httpStatus' - The response's http status code.
newEstimateTemplateCostResponse ::
  -- | 'httpStatus'
  Core.Int ->
  EstimateTemplateCostResponse
newEstimateTemplateCostResponse pHttpStatus_ =
  EstimateTemplateCostResponse'
    { url = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An AWS Simple Monthly Calculator URL with a query string that describes
-- the resources required to run the template.
estimateTemplateCostResponse_url :: Lens.Lens' EstimateTemplateCostResponse (Core.Maybe Core.Text)
estimateTemplateCostResponse_url = Lens.lens (\EstimateTemplateCostResponse' {url} -> url) (\s@EstimateTemplateCostResponse' {} a -> s {url = a} :: EstimateTemplateCostResponse)

-- | The response's http status code.
estimateTemplateCostResponse_httpStatus :: Lens.Lens' EstimateTemplateCostResponse Core.Int
estimateTemplateCostResponse_httpStatus = Lens.lens (\EstimateTemplateCostResponse' {httpStatus} -> httpStatus) (\s@EstimateTemplateCostResponse' {} a -> s {httpStatus = a} :: EstimateTemplateCostResponse)

instance Core.NFData EstimateTemplateCostResponse
