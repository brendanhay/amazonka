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
-- Module      : Amazonka.SageMaker.Types.DriftCheckBias
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DriftCheckBias where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.FileSource
import Amazonka.SageMaker.Types.MetricsSource

-- | Represents the drift check bias baselines that can be used when the
-- model monitor is set using the model package.
--
-- /See:/ 'newDriftCheckBias' smart constructor.
data DriftCheckBias = DriftCheckBias'
  { -- | The post-training constraints.
    postTrainingConstraints :: Prelude.Maybe MetricsSource,
    -- | The pre-training constraints.
    preTrainingConstraints :: Prelude.Maybe MetricsSource,
    -- | The bias config file for a model.
    configFile :: Prelude.Maybe FileSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DriftCheckBias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'postTrainingConstraints', 'driftCheckBias_postTrainingConstraints' - The post-training constraints.
--
-- 'preTrainingConstraints', 'driftCheckBias_preTrainingConstraints' - The pre-training constraints.
--
-- 'configFile', 'driftCheckBias_configFile' - The bias config file for a model.
newDriftCheckBias ::
  DriftCheckBias
newDriftCheckBias =
  DriftCheckBias'
    { postTrainingConstraints =
        Prelude.Nothing,
      preTrainingConstraints = Prelude.Nothing,
      configFile = Prelude.Nothing
    }

-- | The post-training constraints.
driftCheckBias_postTrainingConstraints :: Lens.Lens' DriftCheckBias (Prelude.Maybe MetricsSource)
driftCheckBias_postTrainingConstraints = Lens.lens (\DriftCheckBias' {postTrainingConstraints} -> postTrainingConstraints) (\s@DriftCheckBias' {} a -> s {postTrainingConstraints = a} :: DriftCheckBias)

-- | The pre-training constraints.
driftCheckBias_preTrainingConstraints :: Lens.Lens' DriftCheckBias (Prelude.Maybe MetricsSource)
driftCheckBias_preTrainingConstraints = Lens.lens (\DriftCheckBias' {preTrainingConstraints} -> preTrainingConstraints) (\s@DriftCheckBias' {} a -> s {preTrainingConstraints = a} :: DriftCheckBias)

-- | The bias config file for a model.
driftCheckBias_configFile :: Lens.Lens' DriftCheckBias (Prelude.Maybe FileSource)
driftCheckBias_configFile = Lens.lens (\DriftCheckBias' {configFile} -> configFile) (\s@DriftCheckBias' {} a -> s {configFile = a} :: DriftCheckBias)

instance Data.FromJSON DriftCheckBias where
  parseJSON =
    Data.withObject
      "DriftCheckBias"
      ( \x ->
          DriftCheckBias'
            Prelude.<$> (x Data..:? "PostTrainingConstraints")
            Prelude.<*> (x Data..:? "PreTrainingConstraints")
            Prelude.<*> (x Data..:? "ConfigFile")
      )

instance Prelude.Hashable DriftCheckBias where
  hashWithSalt _salt DriftCheckBias' {..} =
    _salt
      `Prelude.hashWithSalt` postTrainingConstraints
      `Prelude.hashWithSalt` preTrainingConstraints
      `Prelude.hashWithSalt` configFile

instance Prelude.NFData DriftCheckBias where
  rnf DriftCheckBias' {..} =
    Prelude.rnf postTrainingConstraints
      `Prelude.seq` Prelude.rnf preTrainingConstraints
      `Prelude.seq` Prelude.rnf configFile

instance Data.ToJSON DriftCheckBias where
  toJSON DriftCheckBias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PostTrainingConstraints" Data..=)
              Prelude.<$> postTrainingConstraints,
            ("PreTrainingConstraints" Data..=)
              Prelude.<$> preTrainingConstraints,
            ("ConfigFile" Data..=) Prelude.<$> configFile
          ]
      )
