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
-- Module      : Amazonka.Personalize.Types.Algorithm
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.Algorithm where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Personalize.Types.AlgorithmImage
import Amazonka.Personalize.Types.DefaultHyperParameterRanges
import qualified Amazonka.Prelude as Prelude

-- | Describes a custom algorithm.
--
-- /See:/ 'newAlgorithm' smart constructor.
data Algorithm = Algorithm'
  { -- | The URI of the Docker container for the algorithm image.
    algorithmImage :: Prelude.Maybe AlgorithmImage,
    -- | The name of the algorithm.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the role.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the default hyperparameters, their ranges, and whether they
    -- are tunable. A tunable hyperparameter can have its value determined
    -- during hyperparameter optimization (HPO).
    defaultHyperParameterRanges :: Prelude.Maybe DefaultHyperParameterRanges,
    -- | The date and time (in Unix time) that the algorithm was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the algorithm.
    algorithmArn :: Prelude.Maybe Prelude.Text,
    -- | The training input mode.
    trainingInputMode :: Prelude.Maybe Prelude.Text,
    -- | Specifies the default maximum number of training jobs and parallel
    -- training jobs.
    defaultResourceConfig :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies the default hyperparameters.
    defaultHyperParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The date and time (in Unix time) that the algorithm was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Algorithm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmImage', 'algorithm_algorithmImage' - The URI of the Docker container for the algorithm image.
--
-- 'name', 'algorithm_name' - The name of the algorithm.
--
-- 'roleArn', 'algorithm_roleArn' - The Amazon Resource Name (ARN) of the role.
--
-- 'defaultHyperParameterRanges', 'algorithm_defaultHyperParameterRanges' - Specifies the default hyperparameters, their ranges, and whether they
-- are tunable. A tunable hyperparameter can have its value determined
-- during hyperparameter optimization (HPO).
--
-- 'creationDateTime', 'algorithm_creationDateTime' - The date and time (in Unix time) that the algorithm was created.
--
-- 'algorithmArn', 'algorithm_algorithmArn' - The Amazon Resource Name (ARN) of the algorithm.
--
-- 'trainingInputMode', 'algorithm_trainingInputMode' - The training input mode.
--
-- 'defaultResourceConfig', 'algorithm_defaultResourceConfig' - Specifies the default maximum number of training jobs and parallel
-- training jobs.
--
-- 'defaultHyperParameters', 'algorithm_defaultHyperParameters' - Specifies the default hyperparameters.
--
-- 'lastUpdatedDateTime', 'algorithm_lastUpdatedDateTime' - The date and time (in Unix time) that the algorithm was last updated.
newAlgorithm ::
  Algorithm
newAlgorithm =
  Algorithm'
    { algorithmImage = Prelude.Nothing,
      name = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      defaultHyperParameterRanges = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      algorithmArn = Prelude.Nothing,
      trainingInputMode = Prelude.Nothing,
      defaultResourceConfig = Prelude.Nothing,
      defaultHyperParameters = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing
    }

-- | The URI of the Docker container for the algorithm image.
algorithm_algorithmImage :: Lens.Lens' Algorithm (Prelude.Maybe AlgorithmImage)
algorithm_algorithmImage = Lens.lens (\Algorithm' {algorithmImage} -> algorithmImage) (\s@Algorithm' {} a -> s {algorithmImage = a} :: Algorithm)

-- | The name of the algorithm.
algorithm_name :: Lens.Lens' Algorithm (Prelude.Maybe Prelude.Text)
algorithm_name = Lens.lens (\Algorithm' {name} -> name) (\s@Algorithm' {} a -> s {name = a} :: Algorithm)

-- | The Amazon Resource Name (ARN) of the role.
algorithm_roleArn :: Lens.Lens' Algorithm (Prelude.Maybe Prelude.Text)
algorithm_roleArn = Lens.lens (\Algorithm' {roleArn} -> roleArn) (\s@Algorithm' {} a -> s {roleArn = a} :: Algorithm)

-- | Specifies the default hyperparameters, their ranges, and whether they
-- are tunable. A tunable hyperparameter can have its value determined
-- during hyperparameter optimization (HPO).
algorithm_defaultHyperParameterRanges :: Lens.Lens' Algorithm (Prelude.Maybe DefaultHyperParameterRanges)
algorithm_defaultHyperParameterRanges = Lens.lens (\Algorithm' {defaultHyperParameterRanges} -> defaultHyperParameterRanges) (\s@Algorithm' {} a -> s {defaultHyperParameterRanges = a} :: Algorithm)

-- | The date and time (in Unix time) that the algorithm was created.
algorithm_creationDateTime :: Lens.Lens' Algorithm (Prelude.Maybe Prelude.UTCTime)
algorithm_creationDateTime = Lens.lens (\Algorithm' {creationDateTime} -> creationDateTime) (\s@Algorithm' {} a -> s {creationDateTime = a} :: Algorithm) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the algorithm.
algorithm_algorithmArn :: Lens.Lens' Algorithm (Prelude.Maybe Prelude.Text)
algorithm_algorithmArn = Lens.lens (\Algorithm' {algorithmArn} -> algorithmArn) (\s@Algorithm' {} a -> s {algorithmArn = a} :: Algorithm)

-- | The training input mode.
algorithm_trainingInputMode :: Lens.Lens' Algorithm (Prelude.Maybe Prelude.Text)
algorithm_trainingInputMode = Lens.lens (\Algorithm' {trainingInputMode} -> trainingInputMode) (\s@Algorithm' {} a -> s {trainingInputMode = a} :: Algorithm)

-- | Specifies the default maximum number of training jobs and parallel
-- training jobs.
algorithm_defaultResourceConfig :: Lens.Lens' Algorithm (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
algorithm_defaultResourceConfig = Lens.lens (\Algorithm' {defaultResourceConfig} -> defaultResourceConfig) (\s@Algorithm' {} a -> s {defaultResourceConfig = a} :: Algorithm) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the default hyperparameters.
algorithm_defaultHyperParameters :: Lens.Lens' Algorithm (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
algorithm_defaultHyperParameters = Lens.lens (\Algorithm' {defaultHyperParameters} -> defaultHyperParameters) (\s@Algorithm' {} a -> s {defaultHyperParameters = a} :: Algorithm) Prelude.. Lens.mapping Lens.coerced

-- | The date and time (in Unix time) that the algorithm was last updated.
algorithm_lastUpdatedDateTime :: Lens.Lens' Algorithm (Prelude.Maybe Prelude.UTCTime)
algorithm_lastUpdatedDateTime = Lens.lens (\Algorithm' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@Algorithm' {} a -> s {lastUpdatedDateTime = a} :: Algorithm) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Algorithm where
  parseJSON =
    Core.withObject
      "Algorithm"
      ( \x ->
          Algorithm'
            Prelude.<$> (x Core..:? "algorithmImage")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "roleArn")
            Prelude.<*> (x Core..:? "defaultHyperParameterRanges")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "algorithmArn")
            Prelude.<*> (x Core..:? "trainingInputMode")
            Prelude.<*> ( x Core..:? "defaultResourceConfig"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "defaultHyperParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
      )

instance Prelude.Hashable Algorithm where
  hashWithSalt _salt Algorithm' {..} =
    _salt `Prelude.hashWithSalt` algorithmImage
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` defaultHyperParameterRanges
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` algorithmArn
      `Prelude.hashWithSalt` trainingInputMode
      `Prelude.hashWithSalt` defaultResourceConfig
      `Prelude.hashWithSalt` defaultHyperParameters
      `Prelude.hashWithSalt` lastUpdatedDateTime

instance Prelude.NFData Algorithm where
  rnf Algorithm' {..} =
    Prelude.rnf algorithmImage
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf defaultHyperParameterRanges
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf algorithmArn
      `Prelude.seq` Prelude.rnf trainingInputMode
      `Prelude.seq` Prelude.rnf defaultResourceConfig
      `Prelude.seq` Prelude.rnf defaultHyperParameters
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
