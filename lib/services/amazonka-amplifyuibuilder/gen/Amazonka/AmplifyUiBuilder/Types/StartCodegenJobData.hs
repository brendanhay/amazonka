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
-- Module      : Amazonka.AmplifyUiBuilder.Types.StartCodegenJobData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.StartCodegenJobData where

import Amazonka.AmplifyUiBuilder.Types.CodegenFeatureFlags
import Amazonka.AmplifyUiBuilder.Types.CodegenJobGenericDataSchema
import Amazonka.AmplifyUiBuilder.Types.CodegenJobRenderConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The code generation job resource configuration.
--
-- /See:/ 'newStartCodegenJobData' smart constructor.
data StartCodegenJobData = StartCodegenJobData'
  { -- | Specifies whether to autogenerate forms in the code generation job.
    autoGenerateForms :: Prelude.Maybe Prelude.Bool,
    -- | The feature flags for a code generation job.
    features :: Prelude.Maybe CodegenFeatureFlags,
    -- | The data schema to use for a code generation job.
    genericDataSchema :: Prelude.Maybe CodegenJobGenericDataSchema,
    -- | One or more key-value pairs to use when tagging the code generation job
    -- data.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The code generation configuration for the codegen job.
    renderConfig :: CodegenJobRenderConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCodegenJobData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoGenerateForms', 'startCodegenJobData_autoGenerateForms' - Specifies whether to autogenerate forms in the code generation job.
--
-- 'features', 'startCodegenJobData_features' - The feature flags for a code generation job.
--
-- 'genericDataSchema', 'startCodegenJobData_genericDataSchema' - The data schema to use for a code generation job.
--
-- 'tags', 'startCodegenJobData_tags' - One or more key-value pairs to use when tagging the code generation job
-- data.
--
-- 'renderConfig', 'startCodegenJobData_renderConfig' - The code generation configuration for the codegen job.
newStartCodegenJobData ::
  -- | 'renderConfig'
  CodegenJobRenderConfig ->
  StartCodegenJobData
newStartCodegenJobData pRenderConfig_ =
  StartCodegenJobData'
    { autoGenerateForms =
        Prelude.Nothing,
      features = Prelude.Nothing,
      genericDataSchema = Prelude.Nothing,
      tags = Prelude.Nothing,
      renderConfig = pRenderConfig_
    }

-- | Specifies whether to autogenerate forms in the code generation job.
startCodegenJobData_autoGenerateForms :: Lens.Lens' StartCodegenJobData (Prelude.Maybe Prelude.Bool)
startCodegenJobData_autoGenerateForms = Lens.lens (\StartCodegenJobData' {autoGenerateForms} -> autoGenerateForms) (\s@StartCodegenJobData' {} a -> s {autoGenerateForms = a} :: StartCodegenJobData)

-- | The feature flags for a code generation job.
startCodegenJobData_features :: Lens.Lens' StartCodegenJobData (Prelude.Maybe CodegenFeatureFlags)
startCodegenJobData_features = Lens.lens (\StartCodegenJobData' {features} -> features) (\s@StartCodegenJobData' {} a -> s {features = a} :: StartCodegenJobData)

-- | The data schema to use for a code generation job.
startCodegenJobData_genericDataSchema :: Lens.Lens' StartCodegenJobData (Prelude.Maybe CodegenJobGenericDataSchema)
startCodegenJobData_genericDataSchema = Lens.lens (\StartCodegenJobData' {genericDataSchema} -> genericDataSchema) (\s@StartCodegenJobData' {} a -> s {genericDataSchema = a} :: StartCodegenJobData)

-- | One or more key-value pairs to use when tagging the code generation job
-- data.
startCodegenJobData_tags :: Lens.Lens' StartCodegenJobData (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startCodegenJobData_tags = Lens.lens (\StartCodegenJobData' {tags} -> tags) (\s@StartCodegenJobData' {} a -> s {tags = a} :: StartCodegenJobData) Prelude.. Lens.mapping Lens.coerced

-- | The code generation configuration for the codegen job.
startCodegenJobData_renderConfig :: Lens.Lens' StartCodegenJobData CodegenJobRenderConfig
startCodegenJobData_renderConfig = Lens.lens (\StartCodegenJobData' {renderConfig} -> renderConfig) (\s@StartCodegenJobData' {} a -> s {renderConfig = a} :: StartCodegenJobData)

instance Prelude.Hashable StartCodegenJobData where
  hashWithSalt _salt StartCodegenJobData' {..} =
    _salt
      `Prelude.hashWithSalt` autoGenerateForms
      `Prelude.hashWithSalt` features
      `Prelude.hashWithSalt` genericDataSchema
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` renderConfig

instance Prelude.NFData StartCodegenJobData where
  rnf StartCodegenJobData' {..} =
    Prelude.rnf autoGenerateForms
      `Prelude.seq` Prelude.rnf features
      `Prelude.seq` Prelude.rnf genericDataSchema
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf renderConfig

instance Data.ToJSON StartCodegenJobData where
  toJSON StartCodegenJobData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("autoGenerateForms" Data..=)
              Prelude.<$> autoGenerateForms,
            ("features" Data..=) Prelude.<$> features,
            ("genericDataSchema" Data..=)
              Prelude.<$> genericDataSchema,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("renderConfig" Data..= renderConfig)
          ]
      )
