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
-- Module      : Amazonka.CloudFormation.EstimateTemplateCost
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the estimated monthly cost of a template. The return value is an
-- Amazon Web Services Simple Monthly Calculator URL with a query string
-- that describes the resources required to run the template.
module Amazonka.CloudFormation.EstimateTemplateCost
  ( -- * Creating a Request
    EstimateTemplateCost (..),
    newEstimateTemplateCost,

    -- * Request Lenses
    estimateTemplateCost_parameters,
    estimateTemplateCost_templateBody,
    estimateTemplateCost_templateURL,

    -- * Destructuring the Response
    EstimateTemplateCostResponse (..),
    newEstimateTemplateCostResponse,

    -- * Response Lenses
    estimateTemplateCostResponse_url,
    estimateTemplateCostResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for an EstimateTemplateCost action.
--
-- /See:/ 'newEstimateTemplateCost' smart constructor.
data EstimateTemplateCost = EstimateTemplateCost'
  { -- | A list of @Parameter@ structures that specify input parameters.
    parameters :: Prelude.Maybe [Parameter],
    -- | Structure containing the template body with a minimum length of 1 byte
    -- and a maximum length of 51,200 bytes. (For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the CloudFormation User Guide.)
    --
    -- Conditional: You must pass @TemplateBody@ or @TemplateURL@. If both are
    -- passed, only @TemplateBody@ is used.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | Location of file containing the template body. The URL must point to a
    -- template that\'s located in an Amazon S3 bucket or a Systems Manager
    -- document. For more information, go to
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
    -- in the CloudFormation User Guide.
    --
    -- Conditional: You must pass @TemplateURL@ or @TemplateBody@. If both are
    -- passed, only @TemplateBody@ is used.
    templateURL :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EstimateTemplateCost' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'estimateTemplateCost_parameters' - A list of @Parameter@ structures that specify input parameters.
--
-- 'templateBody', 'estimateTemplateCost_templateBody' - Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the CloudFormation User Guide.)
--
-- Conditional: You must pass @TemplateBody@ or @TemplateURL@. If both are
-- passed, only @TemplateBody@ is used.
--
-- 'templateURL', 'estimateTemplateCost_templateURL' - Location of file containing the template body. The URL must point to a
-- template that\'s located in an Amazon S3 bucket or a Systems Manager
-- document. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@. If both are
-- passed, only @TemplateBody@ is used.
newEstimateTemplateCost ::
  EstimateTemplateCost
newEstimateTemplateCost =
  EstimateTemplateCost'
    { parameters = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      templateURL = Prelude.Nothing
    }

-- | A list of @Parameter@ structures that specify input parameters.
estimateTemplateCost_parameters :: Lens.Lens' EstimateTemplateCost (Prelude.Maybe [Parameter])
estimateTemplateCost_parameters = Lens.lens (\EstimateTemplateCost' {parameters} -> parameters) (\s@EstimateTemplateCost' {} a -> s {parameters = a} :: EstimateTemplateCost) Prelude.. Lens.mapping Lens.coerced

-- | Structure containing the template body with a minimum length of 1 byte
-- and a maximum length of 51,200 bytes. (For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the CloudFormation User Guide.)
--
-- Conditional: You must pass @TemplateBody@ or @TemplateURL@. If both are
-- passed, only @TemplateBody@ is used.
estimateTemplateCost_templateBody :: Lens.Lens' EstimateTemplateCost (Prelude.Maybe Prelude.Text)
estimateTemplateCost_templateBody = Lens.lens (\EstimateTemplateCost' {templateBody} -> templateBody) (\s@EstimateTemplateCost' {} a -> s {templateBody = a} :: EstimateTemplateCost)

-- | Location of file containing the template body. The URL must point to a
-- template that\'s located in an Amazon S3 bucket or a Systems Manager
-- document. For more information, go to
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/template-anatomy.html Template Anatomy>
-- in the CloudFormation User Guide.
--
-- Conditional: You must pass @TemplateURL@ or @TemplateBody@. If both are
-- passed, only @TemplateBody@ is used.
estimateTemplateCost_templateURL :: Lens.Lens' EstimateTemplateCost (Prelude.Maybe Prelude.Text)
estimateTemplateCost_templateURL = Lens.lens (\EstimateTemplateCost' {templateURL} -> templateURL) (\s@EstimateTemplateCost' {} a -> s {templateURL = a} :: EstimateTemplateCost)

instance Core.AWSRequest EstimateTemplateCost where
  type
    AWSResponse EstimateTemplateCost =
      EstimateTemplateCostResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "EstimateTemplateCostResult"
      ( \s h x ->
          EstimateTemplateCostResponse'
            Prelude.<$> (x Data..@? "Url")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EstimateTemplateCost where
  hashWithSalt _salt EstimateTemplateCost' {..} =
    _salt
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` templateBody
      `Prelude.hashWithSalt` templateURL

instance Prelude.NFData EstimateTemplateCost where
  rnf EstimateTemplateCost' {..} =
    Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf templateURL

instance Data.ToHeaders EstimateTemplateCost where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath EstimateTemplateCost where
  toPath = Prelude.const "/"

instance Data.ToQuery EstimateTemplateCost where
  toQuery EstimateTemplateCost' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("EstimateTemplateCost" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "Parameters"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> parameters),
        "TemplateBody" Data.=: templateBody,
        "TemplateURL" Data.=: templateURL
      ]

-- | The output for a EstimateTemplateCost action.
--
-- /See:/ 'newEstimateTemplateCostResponse' smart constructor.
data EstimateTemplateCostResponse = EstimateTemplateCostResponse'
  { -- | An Amazon Web Services Simple Monthly Calculator URL with a query string
    -- that describes the resources required to run the template.
    url :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EstimateTemplateCostResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'estimateTemplateCostResponse_url' - An Amazon Web Services Simple Monthly Calculator URL with a query string
-- that describes the resources required to run the template.
--
-- 'httpStatus', 'estimateTemplateCostResponse_httpStatus' - The response's http status code.
newEstimateTemplateCostResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EstimateTemplateCostResponse
newEstimateTemplateCostResponse pHttpStatus_ =
  EstimateTemplateCostResponse'
    { url =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An Amazon Web Services Simple Monthly Calculator URL with a query string
-- that describes the resources required to run the template.
estimateTemplateCostResponse_url :: Lens.Lens' EstimateTemplateCostResponse (Prelude.Maybe Prelude.Text)
estimateTemplateCostResponse_url = Lens.lens (\EstimateTemplateCostResponse' {url} -> url) (\s@EstimateTemplateCostResponse' {} a -> s {url = a} :: EstimateTemplateCostResponse)

-- | The response's http status code.
estimateTemplateCostResponse_httpStatus :: Lens.Lens' EstimateTemplateCostResponse Prelude.Int
estimateTemplateCostResponse_httpStatus = Lens.lens (\EstimateTemplateCostResponse' {httpStatus} -> httpStatus) (\s@EstimateTemplateCostResponse' {} a -> s {httpStatus = a} :: EstimateTemplateCostResponse)

instance Prelude.NFData EstimateTemplateCostResponse where
  rnf EstimateTemplateCostResponse' {..} =
    Prelude.rnf url
      `Prelude.seq` Prelude.rnf httpStatus
