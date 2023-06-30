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
-- Module      : Amazonka.Proton.Types.TemplateSyncConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.TemplateSyncConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.RepositoryProvider
import Amazonka.Proton.Types.TemplateType

-- | The detail data for a template sync configuration.
--
-- /See:/ 'newTemplateSyncConfig' smart constructor.
data TemplateSyncConfig = TemplateSyncConfig'
  { -- | A subdirectory path to your template bundle version.
    subdirectory :: Prelude.Maybe Prelude.Text,
    -- | The repository branch.
    branch :: Prelude.Text,
    -- | The repository name (for example, @myrepos\/myrepo@).
    repositoryName :: Prelude.Text,
    -- | The repository provider.
    repositoryProvider :: RepositoryProvider,
    -- | The template name.
    templateName :: Prelude.Text,
    -- | The template type.
    templateType :: TemplateType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateSyncConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subdirectory', 'templateSyncConfig_subdirectory' - A subdirectory path to your template bundle version.
--
-- 'branch', 'templateSyncConfig_branch' - The repository branch.
--
-- 'repositoryName', 'templateSyncConfig_repositoryName' - The repository name (for example, @myrepos\/myrepo@).
--
-- 'repositoryProvider', 'templateSyncConfig_repositoryProvider' - The repository provider.
--
-- 'templateName', 'templateSyncConfig_templateName' - The template name.
--
-- 'templateType', 'templateSyncConfig_templateType' - The template type.
newTemplateSyncConfig ::
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
  TemplateSyncConfig
newTemplateSyncConfig
  pBranch_
  pRepositoryName_
  pRepositoryProvider_
  pTemplateName_
  pTemplateType_ =
    TemplateSyncConfig'
      { subdirectory = Prelude.Nothing,
        branch = pBranch_,
        repositoryName = pRepositoryName_,
        repositoryProvider = pRepositoryProvider_,
        templateName = pTemplateName_,
        templateType = pTemplateType_
      }

-- | A subdirectory path to your template bundle version.
templateSyncConfig_subdirectory :: Lens.Lens' TemplateSyncConfig (Prelude.Maybe Prelude.Text)
templateSyncConfig_subdirectory = Lens.lens (\TemplateSyncConfig' {subdirectory} -> subdirectory) (\s@TemplateSyncConfig' {} a -> s {subdirectory = a} :: TemplateSyncConfig)

-- | The repository branch.
templateSyncConfig_branch :: Lens.Lens' TemplateSyncConfig Prelude.Text
templateSyncConfig_branch = Lens.lens (\TemplateSyncConfig' {branch} -> branch) (\s@TemplateSyncConfig' {} a -> s {branch = a} :: TemplateSyncConfig)

-- | The repository name (for example, @myrepos\/myrepo@).
templateSyncConfig_repositoryName :: Lens.Lens' TemplateSyncConfig Prelude.Text
templateSyncConfig_repositoryName = Lens.lens (\TemplateSyncConfig' {repositoryName} -> repositoryName) (\s@TemplateSyncConfig' {} a -> s {repositoryName = a} :: TemplateSyncConfig)

-- | The repository provider.
templateSyncConfig_repositoryProvider :: Lens.Lens' TemplateSyncConfig RepositoryProvider
templateSyncConfig_repositoryProvider = Lens.lens (\TemplateSyncConfig' {repositoryProvider} -> repositoryProvider) (\s@TemplateSyncConfig' {} a -> s {repositoryProvider = a} :: TemplateSyncConfig)

-- | The template name.
templateSyncConfig_templateName :: Lens.Lens' TemplateSyncConfig Prelude.Text
templateSyncConfig_templateName = Lens.lens (\TemplateSyncConfig' {templateName} -> templateName) (\s@TemplateSyncConfig' {} a -> s {templateName = a} :: TemplateSyncConfig)

-- | The template type.
templateSyncConfig_templateType :: Lens.Lens' TemplateSyncConfig TemplateType
templateSyncConfig_templateType = Lens.lens (\TemplateSyncConfig' {templateType} -> templateType) (\s@TemplateSyncConfig' {} a -> s {templateType = a} :: TemplateSyncConfig)

instance Data.FromJSON TemplateSyncConfig where
  parseJSON =
    Data.withObject
      "TemplateSyncConfig"
      ( \x ->
          TemplateSyncConfig'
            Prelude.<$> (x Data..:? "subdirectory")
            Prelude.<*> (x Data..: "branch")
            Prelude.<*> (x Data..: "repositoryName")
            Prelude.<*> (x Data..: "repositoryProvider")
            Prelude.<*> (x Data..: "templateName")
            Prelude.<*> (x Data..: "templateType")
      )

instance Prelude.Hashable TemplateSyncConfig where
  hashWithSalt _salt TemplateSyncConfig' {..} =
    _salt
      `Prelude.hashWithSalt` subdirectory
      `Prelude.hashWithSalt` branch
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` repositoryProvider
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateType

instance Prelude.NFData TemplateSyncConfig where
  rnf TemplateSyncConfig' {..} =
    Prelude.rnf subdirectory
      `Prelude.seq` Prelude.rnf branch
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf repositoryProvider
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateType
