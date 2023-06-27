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
-- Module      : Amazonka.SageMaker.Types.ModelConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.EnvironmentParameter

-- | Defines the model configuration. Includes the specification name and
-- environment parameters.
--
-- /See:/ 'newModelConfiguration' smart constructor.
data ModelConfiguration = ModelConfiguration'
  { -- | The name of the compilation job used to create the recommended model
    -- artifacts.
    compilationJobName :: Prelude.Maybe Prelude.Text,
    -- | Defines the environment parameters that includes key, value types, and
    -- values.
    environmentParameters :: Prelude.Maybe (Prelude.NonEmpty EnvironmentParameter),
    -- | The inference specification name in the model package version.
    inferenceSpecificationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compilationJobName', 'modelConfiguration_compilationJobName' - The name of the compilation job used to create the recommended model
-- artifacts.
--
-- 'environmentParameters', 'modelConfiguration_environmentParameters' - Defines the environment parameters that includes key, value types, and
-- values.
--
-- 'inferenceSpecificationName', 'modelConfiguration_inferenceSpecificationName' - The inference specification name in the model package version.
newModelConfiguration ::
  ModelConfiguration
newModelConfiguration =
  ModelConfiguration'
    { compilationJobName =
        Prelude.Nothing,
      environmentParameters = Prelude.Nothing,
      inferenceSpecificationName = Prelude.Nothing
    }

-- | The name of the compilation job used to create the recommended model
-- artifacts.
modelConfiguration_compilationJobName :: Lens.Lens' ModelConfiguration (Prelude.Maybe Prelude.Text)
modelConfiguration_compilationJobName = Lens.lens (\ModelConfiguration' {compilationJobName} -> compilationJobName) (\s@ModelConfiguration' {} a -> s {compilationJobName = a} :: ModelConfiguration)

-- | Defines the environment parameters that includes key, value types, and
-- values.
modelConfiguration_environmentParameters :: Lens.Lens' ModelConfiguration (Prelude.Maybe (Prelude.NonEmpty EnvironmentParameter))
modelConfiguration_environmentParameters = Lens.lens (\ModelConfiguration' {environmentParameters} -> environmentParameters) (\s@ModelConfiguration' {} a -> s {environmentParameters = a} :: ModelConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The inference specification name in the model package version.
modelConfiguration_inferenceSpecificationName :: Lens.Lens' ModelConfiguration (Prelude.Maybe Prelude.Text)
modelConfiguration_inferenceSpecificationName = Lens.lens (\ModelConfiguration' {inferenceSpecificationName} -> inferenceSpecificationName) (\s@ModelConfiguration' {} a -> s {inferenceSpecificationName = a} :: ModelConfiguration)

instance Data.FromJSON ModelConfiguration where
  parseJSON =
    Data.withObject
      "ModelConfiguration"
      ( \x ->
          ModelConfiguration'
            Prelude.<$> (x Data..:? "CompilationJobName")
            Prelude.<*> (x Data..:? "EnvironmentParameters")
            Prelude.<*> (x Data..:? "InferenceSpecificationName")
      )

instance Prelude.Hashable ModelConfiguration where
  hashWithSalt _salt ModelConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` compilationJobName
      `Prelude.hashWithSalt` environmentParameters
      `Prelude.hashWithSalt` inferenceSpecificationName

instance Prelude.NFData ModelConfiguration where
  rnf ModelConfiguration' {..} =
    Prelude.rnf compilationJobName
      `Prelude.seq` Prelude.rnf environmentParameters
      `Prelude.seq` Prelude.rnf inferenceSpecificationName
