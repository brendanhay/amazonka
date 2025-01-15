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
-- Module      : Amazonka.SageMaker.Types.ModelMetadataSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelMetadataSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of the model metadata.
--
-- /See:/ 'newModelMetadataSummary' smart constructor.
data ModelMetadataSummary = ModelMetadataSummary'
  { -- | The machine learning domain of the model.
    domain :: Prelude.Text,
    -- | The machine learning framework of the model.
    framework :: Prelude.Text,
    -- | The machine learning task of the model.
    task :: Prelude.Text,
    -- | The name of the model.
    model :: Prelude.Text,
    -- | The framework version of the model.
    frameworkVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelMetadataSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'modelMetadataSummary_domain' - The machine learning domain of the model.
--
-- 'framework', 'modelMetadataSummary_framework' - The machine learning framework of the model.
--
-- 'task', 'modelMetadataSummary_task' - The machine learning task of the model.
--
-- 'model', 'modelMetadataSummary_model' - The name of the model.
--
-- 'frameworkVersion', 'modelMetadataSummary_frameworkVersion' - The framework version of the model.
newModelMetadataSummary ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'framework'
  Prelude.Text ->
  -- | 'task'
  Prelude.Text ->
  -- | 'model'
  Prelude.Text ->
  -- | 'frameworkVersion'
  Prelude.Text ->
  ModelMetadataSummary
newModelMetadataSummary
  pDomain_
  pFramework_
  pTask_
  pModel_
  pFrameworkVersion_ =
    ModelMetadataSummary'
      { domain = pDomain_,
        framework = pFramework_,
        task = pTask_,
        model = pModel_,
        frameworkVersion = pFrameworkVersion_
      }

-- | The machine learning domain of the model.
modelMetadataSummary_domain :: Lens.Lens' ModelMetadataSummary Prelude.Text
modelMetadataSummary_domain = Lens.lens (\ModelMetadataSummary' {domain} -> domain) (\s@ModelMetadataSummary' {} a -> s {domain = a} :: ModelMetadataSummary)

-- | The machine learning framework of the model.
modelMetadataSummary_framework :: Lens.Lens' ModelMetadataSummary Prelude.Text
modelMetadataSummary_framework = Lens.lens (\ModelMetadataSummary' {framework} -> framework) (\s@ModelMetadataSummary' {} a -> s {framework = a} :: ModelMetadataSummary)

-- | The machine learning task of the model.
modelMetadataSummary_task :: Lens.Lens' ModelMetadataSummary Prelude.Text
modelMetadataSummary_task = Lens.lens (\ModelMetadataSummary' {task} -> task) (\s@ModelMetadataSummary' {} a -> s {task = a} :: ModelMetadataSummary)

-- | The name of the model.
modelMetadataSummary_model :: Lens.Lens' ModelMetadataSummary Prelude.Text
modelMetadataSummary_model = Lens.lens (\ModelMetadataSummary' {model} -> model) (\s@ModelMetadataSummary' {} a -> s {model = a} :: ModelMetadataSummary)

-- | The framework version of the model.
modelMetadataSummary_frameworkVersion :: Lens.Lens' ModelMetadataSummary Prelude.Text
modelMetadataSummary_frameworkVersion = Lens.lens (\ModelMetadataSummary' {frameworkVersion} -> frameworkVersion) (\s@ModelMetadataSummary' {} a -> s {frameworkVersion = a} :: ModelMetadataSummary)

instance Data.FromJSON ModelMetadataSummary where
  parseJSON =
    Data.withObject
      "ModelMetadataSummary"
      ( \x ->
          ModelMetadataSummary'
            Prelude.<$> (x Data..: "Domain")
            Prelude.<*> (x Data..: "Framework")
            Prelude.<*> (x Data..: "Task")
            Prelude.<*> (x Data..: "Model")
            Prelude.<*> (x Data..: "FrameworkVersion")
      )

instance Prelude.Hashable ModelMetadataSummary where
  hashWithSalt _salt ModelMetadataSummary' {..} =
    _salt
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` framework
      `Prelude.hashWithSalt` task
      `Prelude.hashWithSalt` model
      `Prelude.hashWithSalt` frameworkVersion

instance Prelude.NFData ModelMetadataSummary where
  rnf ModelMetadataSummary' {..} =
    Prelude.rnf domain `Prelude.seq`
      Prelude.rnf framework `Prelude.seq`
        Prelude.rnf task `Prelude.seq`
          Prelude.rnf model `Prelude.seq`
            Prelude.rnf frameworkVersion
