{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AmplifyUiBuilder.Types.CodegenJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.CodegenJob where

import Amazonka.AmplifyUiBuilder.Types.CodegenFeatureFlags
import Amazonka.AmplifyUiBuilder.Types.CodegenJobAsset
import Amazonka.AmplifyUiBuilder.Types.CodegenJobGenericDataSchema
import Amazonka.AmplifyUiBuilder.Types.CodegenJobRenderConfig
import Amazonka.AmplifyUiBuilder.Types.CodegenJobStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration for a code generation job that is associated
-- with an Amplify app.
--
-- /See:/ 'newCodegenJob' smart constructor.
data CodegenJob = CodegenJob'
  { -- | The @CodegenJobAsset@ to use for the code generation job.
    asset :: Prelude.Maybe CodegenJobAsset,
    -- | Specifies whether to autogenerate forms in the code generation job.
    autoGenerateForms :: Prelude.Maybe Prelude.Bool,
    -- | The time that the code generation job was created.
    createdAt :: Prelude.Maybe Data.ISO8601,
    features :: Prelude.Maybe CodegenFeatureFlags,
    genericDataSchema :: Prelude.Maybe CodegenJobGenericDataSchema,
    -- | The time that the code generation job was modified.
    modifiedAt :: Prelude.Maybe Data.ISO8601,
    renderConfig :: Prelude.Maybe CodegenJobRenderConfig,
    -- | The status of the code generation job.
    status :: Prelude.Maybe CodegenJobStatus,
    -- | The customized status message for the code generation job.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | One or more key-value pairs to use when tagging the code generation job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique ID for the code generation job.
    id :: Prelude.Text,
    -- | The ID of the Amplify app associated with the code generation job.
    appId :: Prelude.Text,
    -- | The name of the backend environment associated with the code generation
    -- job.
    environmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodegenJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'asset', 'codegenJob_asset' - The @CodegenJobAsset@ to use for the code generation job.
--
-- 'autoGenerateForms', 'codegenJob_autoGenerateForms' - Specifies whether to autogenerate forms in the code generation job.
--
-- 'createdAt', 'codegenJob_createdAt' - The time that the code generation job was created.
--
-- 'features', 'codegenJob_features' - Undocumented member.
--
-- 'genericDataSchema', 'codegenJob_genericDataSchema' - Undocumented member.
--
-- 'modifiedAt', 'codegenJob_modifiedAt' - The time that the code generation job was modified.
--
-- 'renderConfig', 'codegenJob_renderConfig' - Undocumented member.
--
-- 'status', 'codegenJob_status' - The status of the code generation job.
--
-- 'statusMessage', 'codegenJob_statusMessage' - The customized status message for the code generation job.
--
-- 'tags', 'codegenJob_tags' - One or more key-value pairs to use when tagging the code generation job.
--
-- 'id', 'codegenJob_id' - The unique ID for the code generation job.
--
-- 'appId', 'codegenJob_appId' - The ID of the Amplify app associated with the code generation job.
--
-- 'environmentName', 'codegenJob_environmentName' - The name of the backend environment associated with the code generation
-- job.
newCodegenJob ::
  -- | 'id'
  Prelude.Text ->
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  CodegenJob
newCodegenJob pId_ pAppId_ pEnvironmentName_ =
  CodegenJob'
    { asset = Prelude.Nothing,
      autoGenerateForms = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      features = Prelude.Nothing,
      genericDataSchema = Prelude.Nothing,
      modifiedAt = Prelude.Nothing,
      renderConfig = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      tags = Prelude.Nothing,
      id = pId_,
      appId = pAppId_,
      environmentName = pEnvironmentName_
    }

-- | The @CodegenJobAsset@ to use for the code generation job.
codegenJob_asset :: Lens.Lens' CodegenJob (Prelude.Maybe CodegenJobAsset)
codegenJob_asset = Lens.lens (\CodegenJob' {asset} -> asset) (\s@CodegenJob' {} a -> s {asset = a} :: CodegenJob)

-- | Specifies whether to autogenerate forms in the code generation job.
codegenJob_autoGenerateForms :: Lens.Lens' CodegenJob (Prelude.Maybe Prelude.Bool)
codegenJob_autoGenerateForms = Lens.lens (\CodegenJob' {autoGenerateForms} -> autoGenerateForms) (\s@CodegenJob' {} a -> s {autoGenerateForms = a} :: CodegenJob)

-- | The time that the code generation job was created.
codegenJob_createdAt :: Lens.Lens' CodegenJob (Prelude.Maybe Prelude.UTCTime)
codegenJob_createdAt = Lens.lens (\CodegenJob' {createdAt} -> createdAt) (\s@CodegenJob' {} a -> s {createdAt = a} :: CodegenJob) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
codegenJob_features :: Lens.Lens' CodegenJob (Prelude.Maybe CodegenFeatureFlags)
codegenJob_features = Lens.lens (\CodegenJob' {features} -> features) (\s@CodegenJob' {} a -> s {features = a} :: CodegenJob)

-- | Undocumented member.
codegenJob_genericDataSchema :: Lens.Lens' CodegenJob (Prelude.Maybe CodegenJobGenericDataSchema)
codegenJob_genericDataSchema = Lens.lens (\CodegenJob' {genericDataSchema} -> genericDataSchema) (\s@CodegenJob' {} a -> s {genericDataSchema = a} :: CodegenJob)

-- | The time that the code generation job was modified.
codegenJob_modifiedAt :: Lens.Lens' CodegenJob (Prelude.Maybe Prelude.UTCTime)
codegenJob_modifiedAt = Lens.lens (\CodegenJob' {modifiedAt} -> modifiedAt) (\s@CodegenJob' {} a -> s {modifiedAt = a} :: CodegenJob) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
codegenJob_renderConfig :: Lens.Lens' CodegenJob (Prelude.Maybe CodegenJobRenderConfig)
codegenJob_renderConfig = Lens.lens (\CodegenJob' {renderConfig} -> renderConfig) (\s@CodegenJob' {} a -> s {renderConfig = a} :: CodegenJob)

-- | The status of the code generation job.
codegenJob_status :: Lens.Lens' CodegenJob (Prelude.Maybe CodegenJobStatus)
codegenJob_status = Lens.lens (\CodegenJob' {status} -> status) (\s@CodegenJob' {} a -> s {status = a} :: CodegenJob)

-- | The customized status message for the code generation job.
codegenJob_statusMessage :: Lens.Lens' CodegenJob (Prelude.Maybe Prelude.Text)
codegenJob_statusMessage = Lens.lens (\CodegenJob' {statusMessage} -> statusMessage) (\s@CodegenJob' {} a -> s {statusMessage = a} :: CodegenJob)

-- | One or more key-value pairs to use when tagging the code generation job.
codegenJob_tags :: Lens.Lens' CodegenJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
codegenJob_tags = Lens.lens (\CodegenJob' {tags} -> tags) (\s@CodegenJob' {} a -> s {tags = a} :: CodegenJob) Prelude.. Lens.mapping Lens.coerced

-- | The unique ID for the code generation job.
codegenJob_id :: Lens.Lens' CodegenJob Prelude.Text
codegenJob_id = Lens.lens (\CodegenJob' {id} -> id) (\s@CodegenJob' {} a -> s {id = a} :: CodegenJob)

-- | The ID of the Amplify app associated with the code generation job.
codegenJob_appId :: Lens.Lens' CodegenJob Prelude.Text
codegenJob_appId = Lens.lens (\CodegenJob' {appId} -> appId) (\s@CodegenJob' {} a -> s {appId = a} :: CodegenJob)

-- | The name of the backend environment associated with the code generation
-- job.
codegenJob_environmentName :: Lens.Lens' CodegenJob Prelude.Text
codegenJob_environmentName = Lens.lens (\CodegenJob' {environmentName} -> environmentName) (\s@CodegenJob' {} a -> s {environmentName = a} :: CodegenJob)

instance Data.FromJSON CodegenJob where
  parseJSON =
    Data.withObject
      "CodegenJob"
      ( \x ->
          CodegenJob'
            Prelude.<$> (x Data..:? "asset")
            Prelude.<*> (x Data..:? "autoGenerateForms")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "features")
            Prelude.<*> (x Data..:? "genericDataSchema")
            Prelude.<*> (x Data..:? "modifiedAt")
            Prelude.<*> (x Data..:? "renderConfig")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "appId")
            Prelude.<*> (x Data..: "environmentName")
      )

instance Prelude.Hashable CodegenJob where
  hashWithSalt _salt CodegenJob' {..} =
    _salt
      `Prelude.hashWithSalt` asset
      `Prelude.hashWithSalt` autoGenerateForms
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` features
      `Prelude.hashWithSalt` genericDataSchema
      `Prelude.hashWithSalt` modifiedAt
      `Prelude.hashWithSalt` renderConfig
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName

instance Prelude.NFData CodegenJob where
  rnf CodegenJob' {..} =
    Prelude.rnf asset
      `Prelude.seq` Prelude.rnf autoGenerateForms
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf features
      `Prelude.seq` Prelude.rnf genericDataSchema
      `Prelude.seq` Prelude.rnf modifiedAt
      `Prelude.seq` Prelude.rnf renderConfig
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName
