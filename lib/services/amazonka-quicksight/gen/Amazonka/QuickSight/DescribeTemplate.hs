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
-- Module      : Amazonka.QuickSight.DescribeTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a template\'s metadata.
module Amazonka.QuickSight.DescribeTemplate
  ( -- * Creating a Request
    DescribeTemplate (..),
    newDescribeTemplate,

    -- * Request Lenses
    describeTemplate_aliasName,
    describeTemplate_versionNumber,
    describeTemplate_awsAccountId,
    describeTemplate_templateId,

    -- * Destructuring the Response
    DescribeTemplateResponse (..),
    newDescribeTemplateResponse,

    -- * Response Lenses
    describeTemplateResponse_requestId,
    describeTemplateResponse_template,
    describeTemplateResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTemplate' smart constructor.
data DescribeTemplate = DescribeTemplate'
  { -- | The alias of the template that you want to describe. If you name a
    -- specific alias, you describe the version that the alias points to. You
    -- can specify the latest version of the template by providing the keyword
    -- @$LATEST@ in the @AliasName@ parameter. The keyword @$PUBLISHED@
    -- doesn\'t apply to templates.
    aliasName :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The number for the version to describe. If a @VersionNumber@
    -- parameter value isn\'t provided, the latest version of the template is
    -- described.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Amazon Web Services account that contains the template
    -- that you\'re describing.
    awsAccountId :: Prelude.Text,
    -- | The ID for the template.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasName', 'describeTemplate_aliasName' - The alias of the template that you want to describe. If you name a
-- specific alias, you describe the version that the alias points to. You
-- can specify the latest version of the template by providing the keyword
-- @$LATEST@ in the @AliasName@ parameter. The keyword @$PUBLISHED@
-- doesn\'t apply to templates.
--
-- 'versionNumber', 'describeTemplate_versionNumber' - (Optional) The number for the version to describe. If a @VersionNumber@
-- parameter value isn\'t provided, the latest version of the template is
-- described.
--
-- 'awsAccountId', 'describeTemplate_awsAccountId' - The ID of the Amazon Web Services account that contains the template
-- that you\'re describing.
--
-- 'templateId', 'describeTemplate_templateId' - The ID for the template.
newDescribeTemplate ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  DescribeTemplate
newDescribeTemplate pAwsAccountId_ pTemplateId_ =
  DescribeTemplate'
    { aliasName = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      templateId = pTemplateId_
    }

-- | The alias of the template that you want to describe. If you name a
-- specific alias, you describe the version that the alias points to. You
-- can specify the latest version of the template by providing the keyword
-- @$LATEST@ in the @AliasName@ parameter. The keyword @$PUBLISHED@
-- doesn\'t apply to templates.
describeTemplate_aliasName :: Lens.Lens' DescribeTemplate (Prelude.Maybe Prelude.Text)
describeTemplate_aliasName = Lens.lens (\DescribeTemplate' {aliasName} -> aliasName) (\s@DescribeTemplate' {} a -> s {aliasName = a} :: DescribeTemplate)

-- | (Optional) The number for the version to describe. If a @VersionNumber@
-- parameter value isn\'t provided, the latest version of the template is
-- described.
describeTemplate_versionNumber :: Lens.Lens' DescribeTemplate (Prelude.Maybe Prelude.Natural)
describeTemplate_versionNumber = Lens.lens (\DescribeTemplate' {versionNumber} -> versionNumber) (\s@DescribeTemplate' {} a -> s {versionNumber = a} :: DescribeTemplate)

-- | The ID of the Amazon Web Services account that contains the template
-- that you\'re describing.
describeTemplate_awsAccountId :: Lens.Lens' DescribeTemplate Prelude.Text
describeTemplate_awsAccountId = Lens.lens (\DescribeTemplate' {awsAccountId} -> awsAccountId) (\s@DescribeTemplate' {} a -> s {awsAccountId = a} :: DescribeTemplate)

-- | The ID for the template.
describeTemplate_templateId :: Lens.Lens' DescribeTemplate Prelude.Text
describeTemplate_templateId = Lens.lens (\DescribeTemplate' {templateId} -> templateId) (\s@DescribeTemplate' {} a -> s {templateId = a} :: DescribeTemplate)

instance Core.AWSRequest DescribeTemplate where
  type
    AWSResponse DescribeTemplate =
      DescribeTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTemplateResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "Template")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTemplate where
  hashWithSalt _salt DescribeTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` aliasName
      `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` templateId

instance Prelude.NFData DescribeTemplate where
  rnf DescribeTemplate' {..} =
    Prelude.rnf aliasName
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf templateId

instance Data.ToHeaders DescribeTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeTemplate where
  toPath DescribeTemplate' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/templates/",
        Data.toBS templateId
      ]

instance Data.ToQuery DescribeTemplate where
  toQuery DescribeTemplate' {..} =
    Prelude.mconcat
      [ "alias-name" Data.=: aliasName,
        "version-number" Data.=: versionNumber
      ]

-- | /See:/ 'newDescribeTemplateResponse' smart constructor.
data DescribeTemplateResponse = DescribeTemplateResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The template structure for the object you want to describe.
    template :: Prelude.Maybe Template,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'describeTemplateResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'template', 'describeTemplateResponse_template' - The template structure for the object you want to describe.
--
-- 'status', 'describeTemplateResponse_status' - The HTTP status of the request.
newDescribeTemplateResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeTemplateResponse
newDescribeTemplateResponse pStatus_ =
  DescribeTemplateResponse'
    { requestId =
        Prelude.Nothing,
      template = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
describeTemplateResponse_requestId :: Lens.Lens' DescribeTemplateResponse (Prelude.Maybe Prelude.Text)
describeTemplateResponse_requestId = Lens.lens (\DescribeTemplateResponse' {requestId} -> requestId) (\s@DescribeTemplateResponse' {} a -> s {requestId = a} :: DescribeTemplateResponse)

-- | The template structure for the object you want to describe.
describeTemplateResponse_template :: Lens.Lens' DescribeTemplateResponse (Prelude.Maybe Template)
describeTemplateResponse_template = Lens.lens (\DescribeTemplateResponse' {template} -> template) (\s@DescribeTemplateResponse' {} a -> s {template = a} :: DescribeTemplateResponse)

-- | The HTTP status of the request.
describeTemplateResponse_status :: Lens.Lens' DescribeTemplateResponse Prelude.Int
describeTemplateResponse_status = Lens.lens (\DescribeTemplateResponse' {status} -> status) (\s@DescribeTemplateResponse' {} a -> s {status = a} :: DescribeTemplateResponse)

instance Prelude.NFData DescribeTemplateResponse where
  rnf DescribeTemplateResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf template
      `Prelude.seq` Prelude.rnf status
