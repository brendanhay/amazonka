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
-- Module      : Amazonka.AmplifyUiBuilder.Types.CodegenJobRenderConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.CodegenJobRenderConfig where

import Amazonka.AmplifyUiBuilder.Types.ReactStartCodegenJobData
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration information for rendering the UI component
-- associated the code generation job.
--
-- /See:/ 'newCodegenJobRenderConfig' smart constructor.
data CodegenJobRenderConfig = CodegenJobRenderConfig'
  { -- | The name of the @ReactStartCodegenJobData@ object.
    react :: Prelude.Maybe ReactStartCodegenJobData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodegenJobRenderConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'react', 'codegenJobRenderConfig_react' - The name of the @ReactStartCodegenJobData@ object.
newCodegenJobRenderConfig ::
  CodegenJobRenderConfig
newCodegenJobRenderConfig =
  CodegenJobRenderConfig' {react = Prelude.Nothing}

-- | The name of the @ReactStartCodegenJobData@ object.
codegenJobRenderConfig_react :: Lens.Lens' CodegenJobRenderConfig (Prelude.Maybe ReactStartCodegenJobData)
codegenJobRenderConfig_react = Lens.lens (\CodegenJobRenderConfig' {react} -> react) (\s@CodegenJobRenderConfig' {} a -> s {react = a} :: CodegenJobRenderConfig)

instance Data.FromJSON CodegenJobRenderConfig where
  parseJSON =
    Data.withObject
      "CodegenJobRenderConfig"
      ( \x ->
          CodegenJobRenderConfig'
            Prelude.<$> (x Data..:? "react")
      )

instance Prelude.Hashable CodegenJobRenderConfig where
  hashWithSalt _salt CodegenJobRenderConfig' {..} =
    _salt `Prelude.hashWithSalt` react

instance Prelude.NFData CodegenJobRenderConfig where
  rnf CodegenJobRenderConfig' {..} = Prelude.rnf react

instance Data.ToJSON CodegenJobRenderConfig where
  toJSON CodegenJobRenderConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [("react" Data..=) Prelude.<$> react]
      )
