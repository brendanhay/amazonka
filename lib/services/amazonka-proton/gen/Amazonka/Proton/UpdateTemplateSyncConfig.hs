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
-- Module      : Amazonka.Proton.UpdateTemplateSyncConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update template sync configuration parameters, except for the
-- @templateName@ and @templateType@. Repository details (branch, name, and
-- provider) should be of a linked repository. A linked repository is a
-- repository that has been registered with Proton. For more information,
-- see CreateRepository.
module Amazonka.Proton.UpdateTemplateSyncConfig
  ( -- * Creating a Request
    UpdateTemplateSyncConfig (..),
    newUpdateTemplateSyncConfig,

    -- * Request Lenses
    updateTemplateSyncConfig_subdirectory,
    updateTemplateSyncConfig_branch,
    updateTemplateSyncConfig_repositoryName,
    updateTemplateSyncConfig_repositoryProvider,
    updateTemplateSyncConfig_templateName,
    updateTemplateSyncConfig_templateType,

    -- * Destructuring the Response
    UpdateTemplateSyncConfigResponse (..),
    newUpdateTemplateSyncConfigResponse,

    -- * Response Lenses
    updateTemplateSyncConfigResponse_templateSyncConfig,
    updateTemplateSyncConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTemplateSyncConfig' smart constructor.
data UpdateTemplateSyncConfig = UpdateTemplateSyncConfig'
  { -- | A subdirectory path to your template bundle version. When included,
    -- limits the template bundle search to this repository directory.
    subdirectory :: Prelude.Maybe Prelude.Text,
    -- | The repository branch for your template.
    branch :: Prelude.Text,
    -- | The repository name (for example, @myrepos\/myrepo@).
    repositoryName :: Prelude.Text,
    -- | The repository provider.
    repositoryProvider :: RepositoryProvider,
    -- | The synced template name.
    templateName :: Prelude.Text,
    -- | The synced template type.
    templateType :: TemplateType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTemplateSyncConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subdirectory', 'updateTemplateSyncConfig_subdirectory' - A subdirectory path to your template bundle version. When included,
-- limits the template bundle search to this repository directory.
--
-- 'branch', 'updateTemplateSyncConfig_branch' - The repository branch for your template.
--
-- 'repositoryName', 'updateTemplateSyncConfig_repositoryName' - The repository name (for example, @myrepos\/myrepo@).
--
-- 'repositoryProvider', 'updateTemplateSyncConfig_repositoryProvider' - The repository provider.
--
-- 'templateName', 'updateTemplateSyncConfig_templateName' - The synced template name.
--
-- 'templateType', 'updateTemplateSyncConfig_templateType' - The synced template type.
newUpdateTemplateSyncConfig ::
  -- | 'branch'
  Prelude.Text ->
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'repositoryProvider'
  RepositoryProvider ->
  -- | 'templateName'
  Prelude.Text ->
  -- | 'templateType'
  TemplateType ->
  UpdateTemplateSyncConfig
newUpdateTemplateSyncConfig
  pBranch_
  pRepositoryName_
  pRepositoryProvider_
  pTemplateName_
  pTemplateType_ =
    UpdateTemplateSyncConfig'
      { subdirectory =
          Prelude.Nothing,
        branch = pBranch_,
        repositoryName = pRepositoryName_,
        repositoryProvider = pRepositoryProvider_,
        templateName = pTemplateName_,
        templateType = pTemplateType_
      }

-- | A subdirectory path to your template bundle version. When included,
-- limits the template bundle search to this repository directory.
updateTemplateSyncConfig_subdirectory :: Lens.Lens' UpdateTemplateSyncConfig (Prelude.Maybe Prelude.Text)
updateTemplateSyncConfig_subdirectory = Lens.lens (\UpdateTemplateSyncConfig' {subdirectory} -> subdirectory) (\s@UpdateTemplateSyncConfig' {} a -> s {subdirectory = a} :: UpdateTemplateSyncConfig)

-- | The repository branch for your template.
updateTemplateSyncConfig_branch :: Lens.Lens' UpdateTemplateSyncConfig Prelude.Text
updateTemplateSyncConfig_branch = Lens.lens (\UpdateTemplateSyncConfig' {branch} -> branch) (\s@UpdateTemplateSyncConfig' {} a -> s {branch = a} :: UpdateTemplateSyncConfig)

-- | The repository name (for example, @myrepos\/myrepo@).
updateTemplateSyncConfig_repositoryName :: Lens.Lens' UpdateTemplateSyncConfig Prelude.Text
updateTemplateSyncConfig_repositoryName = Lens.lens (\UpdateTemplateSyncConfig' {repositoryName} -> repositoryName) (\s@UpdateTemplateSyncConfig' {} a -> s {repositoryName = a} :: UpdateTemplateSyncConfig)

-- | The repository provider.
updateTemplateSyncConfig_repositoryProvider :: Lens.Lens' UpdateTemplateSyncConfig RepositoryProvider
updateTemplateSyncConfig_repositoryProvider = Lens.lens (\UpdateTemplateSyncConfig' {repositoryProvider} -> repositoryProvider) (\s@UpdateTemplateSyncConfig' {} a -> s {repositoryProvider = a} :: UpdateTemplateSyncConfig)

-- | The synced template name.
updateTemplateSyncConfig_templateName :: Lens.Lens' UpdateTemplateSyncConfig Prelude.Text
updateTemplateSyncConfig_templateName = Lens.lens (\UpdateTemplateSyncConfig' {templateName} -> templateName) (\s@UpdateTemplateSyncConfig' {} a -> s {templateName = a} :: UpdateTemplateSyncConfig)

-- | The synced template type.
updateTemplateSyncConfig_templateType :: Lens.Lens' UpdateTemplateSyncConfig TemplateType
updateTemplateSyncConfig_templateType = Lens.lens (\UpdateTemplateSyncConfig' {templateType} -> templateType) (\s@UpdateTemplateSyncConfig' {} a -> s {templateType = a} :: UpdateTemplateSyncConfig)

instance Core.AWSRequest UpdateTemplateSyncConfig where
  type
    AWSResponse UpdateTemplateSyncConfig =
      UpdateTemplateSyncConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTemplateSyncConfigResponse'
            Prelude.<$> (x Data..?> "templateSyncConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTemplateSyncConfig where
  hashWithSalt _salt UpdateTemplateSyncConfig' {..} =
    _salt
      `Prelude.hashWithSalt` subdirectory
      `Prelude.hashWithSalt` branch
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` repositoryProvider
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateType

instance Prelude.NFData UpdateTemplateSyncConfig where
  rnf UpdateTemplateSyncConfig' {..} =
    Prelude.rnf subdirectory
      `Prelude.seq` Prelude.rnf branch
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf repositoryProvider
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateType

instance Data.ToHeaders UpdateTemplateSyncConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.UpdateTemplateSyncConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTemplateSyncConfig where
  toJSON UpdateTemplateSyncConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("subdirectory" Data..=) Prelude.<$> subdirectory,
            Prelude.Just ("branch" Data..= branch),
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just
              ("repositoryProvider" Data..= repositoryProvider),
            Prelude.Just ("templateName" Data..= templateName),
            Prelude.Just ("templateType" Data..= templateType)
          ]
      )

instance Data.ToPath UpdateTemplateSyncConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateTemplateSyncConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTemplateSyncConfigResponse' smart constructor.
data UpdateTemplateSyncConfigResponse = UpdateTemplateSyncConfigResponse'
  { -- | The template sync configuration detail data that\'s returned by Proton.
    templateSyncConfig :: Prelude.Maybe TemplateSyncConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTemplateSyncConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateSyncConfig', 'updateTemplateSyncConfigResponse_templateSyncConfig' - The template sync configuration detail data that\'s returned by Proton.
--
-- 'httpStatus', 'updateTemplateSyncConfigResponse_httpStatus' - The response's http status code.
newUpdateTemplateSyncConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTemplateSyncConfigResponse
newUpdateTemplateSyncConfigResponse pHttpStatus_ =
  UpdateTemplateSyncConfigResponse'
    { templateSyncConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The template sync configuration detail data that\'s returned by Proton.
updateTemplateSyncConfigResponse_templateSyncConfig :: Lens.Lens' UpdateTemplateSyncConfigResponse (Prelude.Maybe TemplateSyncConfig)
updateTemplateSyncConfigResponse_templateSyncConfig = Lens.lens (\UpdateTemplateSyncConfigResponse' {templateSyncConfig} -> templateSyncConfig) (\s@UpdateTemplateSyncConfigResponse' {} a -> s {templateSyncConfig = a} :: UpdateTemplateSyncConfigResponse)

-- | The response's http status code.
updateTemplateSyncConfigResponse_httpStatus :: Lens.Lens' UpdateTemplateSyncConfigResponse Prelude.Int
updateTemplateSyncConfigResponse_httpStatus = Lens.lens (\UpdateTemplateSyncConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateTemplateSyncConfigResponse' {} a -> s {httpStatus = a} :: UpdateTemplateSyncConfigResponse)

instance
  Prelude.NFData
    UpdateTemplateSyncConfigResponse
  where
  rnf UpdateTemplateSyncConfigResponse' {..} =
    Prelude.rnf templateSyncConfig
      `Prelude.seq` Prelude.rnf httpStatus
