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
-- Module      : Amazonka.QuickSight.CreateTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a template from an existing Amazon QuickSight analysis or
-- template. You can use the resulting template to create a dashboard.
--
-- A /template/ is an entity in Amazon QuickSight that encapsulates the
-- metadata required to create an analysis and that you can use to create s
-- dashboard. A template adds a layer of abstraction by using placeholders
-- to replace the dataset associated with the analysis. You can use
-- templates to create dashboards by replacing dataset placeholders with
-- datasets that follow the same schema that was used to create the source
-- analysis and template.
module Amazonka.QuickSight.CreateTemplate
  ( -- * Creating a Request
    CreateTemplate (..),
    newCreateTemplate,

    -- * Request Lenses
    createTemplate_definition,
    createTemplate_name,
    createTemplate_permissions,
    createTemplate_sourceEntity,
    createTemplate_tags,
    createTemplate_versionDescription,
    createTemplate_awsAccountId,
    createTemplate_templateId,

    -- * Destructuring the Response
    CreateTemplateResponse (..),
    newCreateTemplateResponse,

    -- * Response Lenses
    createTemplateResponse_arn,
    createTemplateResponse_creationStatus,
    createTemplateResponse_requestId,
    createTemplateResponse_templateId,
    createTemplateResponse_versionArn,
    createTemplateResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTemplate' smart constructor.
data CreateTemplate = CreateTemplate'
  { -- | The definition of a template.
    --
    -- A definition is the data model of all features in a Dashboard, Template,
    -- or Analysis.
    definition :: Prelude.Maybe TemplateVersionDefinition,
    -- | A display name for the template.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of resource permissions to be set on the template.
    permissions :: Prelude.Maybe (Prelude.NonEmpty ResourcePermission),
    -- | The entity that you are using as a source when you create the template.
    -- In @SourceEntity@, you specify the type of object you\'re using as
    -- source: @SourceTemplate@ for a template or @SourceAnalysis@ for an
    -- analysis. Both of these require an Amazon Resource Name (ARN). For
    -- @SourceTemplate@, specify the ARN of the source template. For
    -- @SourceAnalysis@, specify the ARN of the source analysis. The
    -- @SourceTemplate@ ARN can contain any Amazon Web Services account and any
    -- Amazon QuickSight-supported Amazon Web Services Region.
    --
    -- Use the @DataSetReferences@ entity within @SourceTemplate@ or
    -- @SourceAnalysis@ to list the replacement datasets for the placeholders
    -- listed in the original. The schema in each dataset must match its
    -- placeholder.
    sourceEntity :: Prelude.Maybe TemplateSourceEntity,
    -- | Contains a map of the key-value pairs for the resource tag or tags
    -- assigned to the resource.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | A description of the current template version being created. This API
    -- operation creates the first version of the template. Every time
    -- @UpdateTemplate@ is called, a new version is created. Each version of
    -- the template maintains a description of the version in the
    -- @VersionDescription@ field.
    versionDescription :: Prelude.Maybe Prelude.Text,
    -- | The ID for the Amazon Web Services account that the group is in. You use
    -- the ID for the Amazon Web Services account that contains your Amazon
    -- QuickSight account.
    awsAccountId :: Prelude.Text,
    -- | An ID for the template that you want to create. This template is unique
    -- per Amazon Web Services Region; in each Amazon Web Services account.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definition', 'createTemplate_definition' - The definition of a template.
--
-- A definition is the data model of all features in a Dashboard, Template,
-- or Analysis.
--
-- 'name', 'createTemplate_name' - A display name for the template.
--
-- 'permissions', 'createTemplate_permissions' - A list of resource permissions to be set on the template.
--
-- 'sourceEntity', 'createTemplate_sourceEntity' - The entity that you are using as a source when you create the template.
-- In @SourceEntity@, you specify the type of object you\'re using as
-- source: @SourceTemplate@ for a template or @SourceAnalysis@ for an
-- analysis. Both of these require an Amazon Resource Name (ARN). For
-- @SourceTemplate@, specify the ARN of the source template. For
-- @SourceAnalysis@, specify the ARN of the source analysis. The
-- @SourceTemplate@ ARN can contain any Amazon Web Services account and any
-- Amazon QuickSight-supported Amazon Web Services Region.
--
-- Use the @DataSetReferences@ entity within @SourceTemplate@ or
-- @SourceAnalysis@ to list the replacement datasets for the placeholders
-- listed in the original. The schema in each dataset must match its
-- placeholder.
--
-- 'tags', 'createTemplate_tags' - Contains a map of the key-value pairs for the resource tag or tags
-- assigned to the resource.
--
-- 'versionDescription', 'createTemplate_versionDescription' - A description of the current template version being created. This API
-- operation creates the first version of the template. Every time
-- @UpdateTemplate@ is called, a new version is created. Each version of
-- the template maintains a description of the version in the
-- @VersionDescription@ field.
--
-- 'awsAccountId', 'createTemplate_awsAccountId' - The ID for the Amazon Web Services account that the group is in. You use
-- the ID for the Amazon Web Services account that contains your Amazon
-- QuickSight account.
--
-- 'templateId', 'createTemplate_templateId' - An ID for the template that you want to create. This template is unique
-- per Amazon Web Services Region; in each Amazon Web Services account.
newCreateTemplate ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  CreateTemplate
newCreateTemplate pAwsAccountId_ pTemplateId_ =
  CreateTemplate'
    { definition = Prelude.Nothing,
      name = Prelude.Nothing,
      permissions = Prelude.Nothing,
      sourceEntity = Prelude.Nothing,
      tags = Prelude.Nothing,
      versionDescription = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      templateId = pTemplateId_
    }

-- | The definition of a template.
--
-- A definition is the data model of all features in a Dashboard, Template,
-- or Analysis.
createTemplate_definition :: Lens.Lens' CreateTemplate (Prelude.Maybe TemplateVersionDefinition)
createTemplate_definition = Lens.lens (\CreateTemplate' {definition} -> definition) (\s@CreateTemplate' {} a -> s {definition = a} :: CreateTemplate)

-- | A display name for the template.
createTemplate_name :: Lens.Lens' CreateTemplate (Prelude.Maybe Prelude.Text)
createTemplate_name = Lens.lens (\CreateTemplate' {name} -> name) (\s@CreateTemplate' {} a -> s {name = a} :: CreateTemplate)

-- | A list of resource permissions to be set on the template.
createTemplate_permissions :: Lens.Lens' CreateTemplate (Prelude.Maybe (Prelude.NonEmpty ResourcePermission))
createTemplate_permissions = Lens.lens (\CreateTemplate' {permissions} -> permissions) (\s@CreateTemplate' {} a -> s {permissions = a} :: CreateTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The entity that you are using as a source when you create the template.
-- In @SourceEntity@, you specify the type of object you\'re using as
-- source: @SourceTemplate@ for a template or @SourceAnalysis@ for an
-- analysis. Both of these require an Amazon Resource Name (ARN). For
-- @SourceTemplate@, specify the ARN of the source template. For
-- @SourceAnalysis@, specify the ARN of the source analysis. The
-- @SourceTemplate@ ARN can contain any Amazon Web Services account and any
-- Amazon QuickSight-supported Amazon Web Services Region.
--
-- Use the @DataSetReferences@ entity within @SourceTemplate@ or
-- @SourceAnalysis@ to list the replacement datasets for the placeholders
-- listed in the original. The schema in each dataset must match its
-- placeholder.
createTemplate_sourceEntity :: Lens.Lens' CreateTemplate (Prelude.Maybe TemplateSourceEntity)
createTemplate_sourceEntity = Lens.lens (\CreateTemplate' {sourceEntity} -> sourceEntity) (\s@CreateTemplate' {} a -> s {sourceEntity = a} :: CreateTemplate)

-- | Contains a map of the key-value pairs for the resource tag or tags
-- assigned to the resource.
createTemplate_tags :: Lens.Lens' CreateTemplate (Prelude.Maybe (Prelude.NonEmpty Tag))
createTemplate_tags = Lens.lens (\CreateTemplate' {tags} -> tags) (\s@CreateTemplate' {} a -> s {tags = a} :: CreateTemplate) Prelude.. Lens.mapping Lens.coerced

-- | A description of the current template version being created. This API
-- operation creates the first version of the template. Every time
-- @UpdateTemplate@ is called, a new version is created. Each version of
-- the template maintains a description of the version in the
-- @VersionDescription@ field.
createTemplate_versionDescription :: Lens.Lens' CreateTemplate (Prelude.Maybe Prelude.Text)
createTemplate_versionDescription = Lens.lens (\CreateTemplate' {versionDescription} -> versionDescription) (\s@CreateTemplate' {} a -> s {versionDescription = a} :: CreateTemplate)

-- | The ID for the Amazon Web Services account that the group is in. You use
-- the ID for the Amazon Web Services account that contains your Amazon
-- QuickSight account.
createTemplate_awsAccountId :: Lens.Lens' CreateTemplate Prelude.Text
createTemplate_awsAccountId = Lens.lens (\CreateTemplate' {awsAccountId} -> awsAccountId) (\s@CreateTemplate' {} a -> s {awsAccountId = a} :: CreateTemplate)

-- | An ID for the template that you want to create. This template is unique
-- per Amazon Web Services Region; in each Amazon Web Services account.
createTemplate_templateId :: Lens.Lens' CreateTemplate Prelude.Text
createTemplate_templateId = Lens.lens (\CreateTemplate' {templateId} -> templateId) (\s@CreateTemplate' {} a -> s {templateId = a} :: CreateTemplate)

instance Core.AWSRequest CreateTemplate where
  type
    AWSResponse CreateTemplate =
      CreateTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTemplateResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationStatus")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "TemplateId")
            Prelude.<*> (x Data..?> "VersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTemplate where
  hashWithSalt _salt CreateTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` permissions
      `Prelude.hashWithSalt` sourceEntity
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` versionDescription
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` templateId

instance Prelude.NFData CreateTemplate where
  rnf CreateTemplate' {..} =
    Prelude.rnf definition `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf permissions `Prelude.seq`
          Prelude.rnf sourceEntity `Prelude.seq`
            Prelude.rnf tags `Prelude.seq`
              Prelude.rnf versionDescription `Prelude.seq`
                Prelude.rnf awsAccountId `Prelude.seq`
                  Prelude.rnf templateId

instance Data.ToHeaders CreateTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTemplate where
  toJSON CreateTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Definition" Data..=) Prelude.<$> definition,
            ("Name" Data..=) Prelude.<$> name,
            ("Permissions" Data..=) Prelude.<$> permissions,
            ("SourceEntity" Data..=) Prelude.<$> sourceEntity,
            ("Tags" Data..=) Prelude.<$> tags,
            ("VersionDescription" Data..=)
              Prelude.<$> versionDescription
          ]
      )

instance Data.ToPath CreateTemplate where
  toPath CreateTemplate' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/templates/",
        Data.toBS templateId
      ]

instance Data.ToQuery CreateTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTemplateResponse' smart constructor.
data CreateTemplateResponse = CreateTemplateResponse'
  { -- | The ARN for the template.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The template creation status.
    creationStatus :: Prelude.Maybe ResourceStatus,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the template.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the template, including the version information of the first
    -- version.
    versionArn :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createTemplateResponse_arn' - The ARN for the template.
--
-- 'creationStatus', 'createTemplateResponse_creationStatus' - The template creation status.
--
-- 'requestId', 'createTemplateResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'templateId', 'createTemplateResponse_templateId' - The ID of the template.
--
-- 'versionArn', 'createTemplateResponse_versionArn' - The ARN for the template, including the version information of the first
-- version.
--
-- 'status', 'createTemplateResponse_status' - The HTTP status of the request.
newCreateTemplateResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateTemplateResponse
newCreateTemplateResponse pStatus_ =
  CreateTemplateResponse'
    { arn = Prelude.Nothing,
      creationStatus = Prelude.Nothing,
      requestId = Prelude.Nothing,
      templateId = Prelude.Nothing,
      versionArn = Prelude.Nothing,
      status = pStatus_
    }

-- | The ARN for the template.
createTemplateResponse_arn :: Lens.Lens' CreateTemplateResponse (Prelude.Maybe Prelude.Text)
createTemplateResponse_arn = Lens.lens (\CreateTemplateResponse' {arn} -> arn) (\s@CreateTemplateResponse' {} a -> s {arn = a} :: CreateTemplateResponse)

-- | The template creation status.
createTemplateResponse_creationStatus :: Lens.Lens' CreateTemplateResponse (Prelude.Maybe ResourceStatus)
createTemplateResponse_creationStatus = Lens.lens (\CreateTemplateResponse' {creationStatus} -> creationStatus) (\s@CreateTemplateResponse' {} a -> s {creationStatus = a} :: CreateTemplateResponse)

-- | The Amazon Web Services request ID for this operation.
createTemplateResponse_requestId :: Lens.Lens' CreateTemplateResponse (Prelude.Maybe Prelude.Text)
createTemplateResponse_requestId = Lens.lens (\CreateTemplateResponse' {requestId} -> requestId) (\s@CreateTemplateResponse' {} a -> s {requestId = a} :: CreateTemplateResponse)

-- | The ID of the template.
createTemplateResponse_templateId :: Lens.Lens' CreateTemplateResponse (Prelude.Maybe Prelude.Text)
createTemplateResponse_templateId = Lens.lens (\CreateTemplateResponse' {templateId} -> templateId) (\s@CreateTemplateResponse' {} a -> s {templateId = a} :: CreateTemplateResponse)

-- | The ARN for the template, including the version information of the first
-- version.
createTemplateResponse_versionArn :: Lens.Lens' CreateTemplateResponse (Prelude.Maybe Prelude.Text)
createTemplateResponse_versionArn = Lens.lens (\CreateTemplateResponse' {versionArn} -> versionArn) (\s@CreateTemplateResponse' {} a -> s {versionArn = a} :: CreateTemplateResponse)

-- | The HTTP status of the request.
createTemplateResponse_status :: Lens.Lens' CreateTemplateResponse Prelude.Int
createTemplateResponse_status = Lens.lens (\CreateTemplateResponse' {status} -> status) (\s@CreateTemplateResponse' {} a -> s {status = a} :: CreateTemplateResponse)

instance Prelude.NFData CreateTemplateResponse where
  rnf CreateTemplateResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationStatus `Prelude.seq`
        Prelude.rnf requestId `Prelude.seq`
          Prelude.rnf templateId `Prelude.seq`
            Prelude.rnf versionArn `Prelude.seq`
              Prelude.rnf status
