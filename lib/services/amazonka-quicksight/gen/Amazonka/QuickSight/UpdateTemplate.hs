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
-- Module      : Amazonka.QuickSight.UpdateTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a template from an existing Amazon QuickSight analysis or
-- another template.
module Amazonka.QuickSight.UpdateTemplate
  ( -- * Creating a Request
    UpdateTemplate (..),
    newUpdateTemplate,

    -- * Request Lenses
    updateTemplate_definition,
    updateTemplate_name,
    updateTemplate_sourceEntity,
    updateTemplate_versionDescription,
    updateTemplate_awsAccountId,
    updateTemplate_templateId,

    -- * Destructuring the Response
    UpdateTemplateResponse (..),
    newUpdateTemplateResponse,

    -- * Response Lenses
    updateTemplateResponse_arn,
    updateTemplateResponse_creationStatus,
    updateTemplateResponse_requestId,
    updateTemplateResponse_templateId,
    updateTemplateResponse_versionArn,
    updateTemplateResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTemplate' smart constructor.
data UpdateTemplate = UpdateTemplate'
  { -- | The definition of a template.
    --
    -- A definition is the data model of all features in a Dashboard, Template,
    -- or Analysis.
    definition :: Prelude.Maybe TemplateVersionDefinition,
    -- | The name for the template.
    name :: Prelude.Maybe Prelude.Text,
    -- | The entity that you are using as a source when you update the template.
    -- In @SourceEntity@, you specify the type of object you\'re using as
    -- source: @SourceTemplate@ for a template or @SourceAnalysis@ for an
    -- analysis. Both of these require an Amazon Resource Name (ARN). For
    -- @SourceTemplate@, specify the ARN of the source template. For
    -- @SourceAnalysis@, specify the ARN of the source analysis. The
    -- @SourceTemplate@ ARN can contain any Amazon Web Services account and any
    -- Amazon QuickSight-supported Amazon Web Services Region;.
    --
    -- Use the @DataSetReferences@ entity within @SourceTemplate@ or
    -- @SourceAnalysis@ to list the replacement datasets for the placeholders
    -- listed in the original. The schema in each dataset must match its
    -- placeholder.
    sourceEntity :: Prelude.Maybe TemplateSourceEntity,
    -- | A description of the current template version that is being updated.
    -- Every time you call @UpdateTemplate@, you create a new version of the
    -- template. Each version of the template maintains a description of the
    -- version in the @VersionDescription@ field.
    versionDescription :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that contains the template
    -- that you\'re updating.
    awsAccountId :: Prelude.Text,
    -- | The ID for the template.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definition', 'updateTemplate_definition' - The definition of a template.
--
-- A definition is the data model of all features in a Dashboard, Template,
-- or Analysis.
--
-- 'name', 'updateTemplate_name' - The name for the template.
--
-- 'sourceEntity', 'updateTemplate_sourceEntity' - The entity that you are using as a source when you update the template.
-- In @SourceEntity@, you specify the type of object you\'re using as
-- source: @SourceTemplate@ for a template or @SourceAnalysis@ for an
-- analysis. Both of these require an Amazon Resource Name (ARN). For
-- @SourceTemplate@, specify the ARN of the source template. For
-- @SourceAnalysis@, specify the ARN of the source analysis. The
-- @SourceTemplate@ ARN can contain any Amazon Web Services account and any
-- Amazon QuickSight-supported Amazon Web Services Region;.
--
-- Use the @DataSetReferences@ entity within @SourceTemplate@ or
-- @SourceAnalysis@ to list the replacement datasets for the placeholders
-- listed in the original. The schema in each dataset must match its
-- placeholder.
--
-- 'versionDescription', 'updateTemplate_versionDescription' - A description of the current template version that is being updated.
-- Every time you call @UpdateTemplate@, you create a new version of the
-- template. Each version of the template maintains a description of the
-- version in the @VersionDescription@ field.
--
-- 'awsAccountId', 'updateTemplate_awsAccountId' - The ID of the Amazon Web Services account that contains the template
-- that you\'re updating.
--
-- 'templateId', 'updateTemplate_templateId' - The ID for the template.
newUpdateTemplate ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  UpdateTemplate
newUpdateTemplate pAwsAccountId_ pTemplateId_ =
  UpdateTemplate'
    { definition = Prelude.Nothing,
      name = Prelude.Nothing,
      sourceEntity = Prelude.Nothing,
      versionDescription = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      templateId = pTemplateId_
    }

-- | The definition of a template.
--
-- A definition is the data model of all features in a Dashboard, Template,
-- or Analysis.
updateTemplate_definition :: Lens.Lens' UpdateTemplate (Prelude.Maybe TemplateVersionDefinition)
updateTemplate_definition = Lens.lens (\UpdateTemplate' {definition} -> definition) (\s@UpdateTemplate' {} a -> s {definition = a} :: UpdateTemplate)

-- | The name for the template.
updateTemplate_name :: Lens.Lens' UpdateTemplate (Prelude.Maybe Prelude.Text)
updateTemplate_name = Lens.lens (\UpdateTemplate' {name} -> name) (\s@UpdateTemplate' {} a -> s {name = a} :: UpdateTemplate)

-- | The entity that you are using as a source when you update the template.
-- In @SourceEntity@, you specify the type of object you\'re using as
-- source: @SourceTemplate@ for a template or @SourceAnalysis@ for an
-- analysis. Both of these require an Amazon Resource Name (ARN). For
-- @SourceTemplate@, specify the ARN of the source template. For
-- @SourceAnalysis@, specify the ARN of the source analysis. The
-- @SourceTemplate@ ARN can contain any Amazon Web Services account and any
-- Amazon QuickSight-supported Amazon Web Services Region;.
--
-- Use the @DataSetReferences@ entity within @SourceTemplate@ or
-- @SourceAnalysis@ to list the replacement datasets for the placeholders
-- listed in the original. The schema in each dataset must match its
-- placeholder.
updateTemplate_sourceEntity :: Lens.Lens' UpdateTemplate (Prelude.Maybe TemplateSourceEntity)
updateTemplate_sourceEntity = Lens.lens (\UpdateTemplate' {sourceEntity} -> sourceEntity) (\s@UpdateTemplate' {} a -> s {sourceEntity = a} :: UpdateTemplate)

-- | A description of the current template version that is being updated.
-- Every time you call @UpdateTemplate@, you create a new version of the
-- template. Each version of the template maintains a description of the
-- version in the @VersionDescription@ field.
updateTemplate_versionDescription :: Lens.Lens' UpdateTemplate (Prelude.Maybe Prelude.Text)
updateTemplate_versionDescription = Lens.lens (\UpdateTemplate' {versionDescription} -> versionDescription) (\s@UpdateTemplate' {} a -> s {versionDescription = a} :: UpdateTemplate)

-- | The ID of the Amazon Web Services account that contains the template
-- that you\'re updating.
updateTemplate_awsAccountId :: Lens.Lens' UpdateTemplate Prelude.Text
updateTemplate_awsAccountId = Lens.lens (\UpdateTemplate' {awsAccountId} -> awsAccountId) (\s@UpdateTemplate' {} a -> s {awsAccountId = a} :: UpdateTemplate)

-- | The ID for the template.
updateTemplate_templateId :: Lens.Lens' UpdateTemplate Prelude.Text
updateTemplate_templateId = Lens.lens (\UpdateTemplate' {templateId} -> templateId) (\s@UpdateTemplate' {} a -> s {templateId = a} :: UpdateTemplate)

instance Core.AWSRequest UpdateTemplate where
  type
    AWSResponse UpdateTemplate =
      UpdateTemplateResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTemplateResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationStatus")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "TemplateId")
            Prelude.<*> (x Data..?> "VersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTemplate where
  hashWithSalt _salt UpdateTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sourceEntity
      `Prelude.hashWithSalt` versionDescription
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` templateId

instance Prelude.NFData UpdateTemplate where
  rnf UpdateTemplate' {..} =
    Prelude.rnf definition
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sourceEntity
      `Prelude.seq` Prelude.rnf versionDescription
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf templateId

instance Data.ToHeaders UpdateTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTemplate where
  toJSON UpdateTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Definition" Data..=) Prelude.<$> definition,
            ("Name" Data..=) Prelude.<$> name,
            ("SourceEntity" Data..=) Prelude.<$> sourceEntity,
            ("VersionDescription" Data..=)
              Prelude.<$> versionDescription
          ]
      )

instance Data.ToPath UpdateTemplate where
  toPath UpdateTemplate' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/templates/",
        Data.toBS templateId
      ]

instance Data.ToQuery UpdateTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTemplateResponse' smart constructor.
data UpdateTemplateResponse = UpdateTemplateResponse'
  { -- | The Amazon Resource Name (ARN) for the template.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The creation status of the template.
    creationStatus :: Prelude.Maybe ResourceStatus,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The ID for the template.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the template, including the version information of the first
    -- version.
    versionArn :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateTemplateResponse_arn' - The Amazon Resource Name (ARN) for the template.
--
-- 'creationStatus', 'updateTemplateResponse_creationStatus' - The creation status of the template.
--
-- 'requestId', 'updateTemplateResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'templateId', 'updateTemplateResponse_templateId' - The ID for the template.
--
-- 'versionArn', 'updateTemplateResponse_versionArn' - The ARN for the template, including the version information of the first
-- version.
--
-- 'status', 'updateTemplateResponse_status' - The HTTP status of the request.
newUpdateTemplateResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateTemplateResponse
newUpdateTemplateResponse pStatus_ =
  UpdateTemplateResponse'
    { arn = Prelude.Nothing,
      creationStatus = Prelude.Nothing,
      requestId = Prelude.Nothing,
      templateId = Prelude.Nothing,
      versionArn = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) for the template.
updateTemplateResponse_arn :: Lens.Lens' UpdateTemplateResponse (Prelude.Maybe Prelude.Text)
updateTemplateResponse_arn = Lens.lens (\UpdateTemplateResponse' {arn} -> arn) (\s@UpdateTemplateResponse' {} a -> s {arn = a} :: UpdateTemplateResponse)

-- | The creation status of the template.
updateTemplateResponse_creationStatus :: Lens.Lens' UpdateTemplateResponse (Prelude.Maybe ResourceStatus)
updateTemplateResponse_creationStatus = Lens.lens (\UpdateTemplateResponse' {creationStatus} -> creationStatus) (\s@UpdateTemplateResponse' {} a -> s {creationStatus = a} :: UpdateTemplateResponse)

-- | The Amazon Web Services request ID for this operation.
updateTemplateResponse_requestId :: Lens.Lens' UpdateTemplateResponse (Prelude.Maybe Prelude.Text)
updateTemplateResponse_requestId = Lens.lens (\UpdateTemplateResponse' {requestId} -> requestId) (\s@UpdateTemplateResponse' {} a -> s {requestId = a} :: UpdateTemplateResponse)

-- | The ID for the template.
updateTemplateResponse_templateId :: Lens.Lens' UpdateTemplateResponse (Prelude.Maybe Prelude.Text)
updateTemplateResponse_templateId = Lens.lens (\UpdateTemplateResponse' {templateId} -> templateId) (\s@UpdateTemplateResponse' {} a -> s {templateId = a} :: UpdateTemplateResponse)

-- | The ARN for the template, including the version information of the first
-- version.
updateTemplateResponse_versionArn :: Lens.Lens' UpdateTemplateResponse (Prelude.Maybe Prelude.Text)
updateTemplateResponse_versionArn = Lens.lens (\UpdateTemplateResponse' {versionArn} -> versionArn) (\s@UpdateTemplateResponse' {} a -> s {versionArn = a} :: UpdateTemplateResponse)

-- | The HTTP status of the request.
updateTemplateResponse_status :: Lens.Lens' UpdateTemplateResponse Prelude.Int
updateTemplateResponse_status = Lens.lens (\UpdateTemplateResponse' {status} -> status) (\s@UpdateTemplateResponse' {} a -> s {status = a} :: UpdateTemplateResponse)

instance Prelude.NFData UpdateTemplateResponse where
  rnf UpdateTemplateResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationStatus
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf versionArn
      `Prelude.seq` Prelude.rnf status
