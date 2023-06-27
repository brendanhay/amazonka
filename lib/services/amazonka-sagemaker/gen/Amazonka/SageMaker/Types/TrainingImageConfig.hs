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
-- Module      : Amazonka.SageMaker.Types.TrainingImageConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrainingImageConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.TrainingRepositoryAccessMode
import Amazonka.SageMaker.Types.TrainingRepositoryAuthConfig

-- | The configuration to use an image from a private Docker registry for a
-- training job.
--
-- /See:/ 'newTrainingImageConfig' smart constructor.
data TrainingImageConfig = TrainingImageConfig'
  { -- | An object containing authentication information for a private Docker
    -- registry containing your training images.
    trainingRepositoryAuthConfig :: Prelude.Maybe TrainingRepositoryAuthConfig,
    -- | The method that your training job will use to gain access to the images
    -- in your private Docker registry. For access to an image in a private
    -- Docker registry, set to @Vpc@.
    trainingRepositoryAccessMode :: TrainingRepositoryAccessMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrainingImageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trainingRepositoryAuthConfig', 'trainingImageConfig_trainingRepositoryAuthConfig' - An object containing authentication information for a private Docker
-- registry containing your training images.
--
-- 'trainingRepositoryAccessMode', 'trainingImageConfig_trainingRepositoryAccessMode' - The method that your training job will use to gain access to the images
-- in your private Docker registry. For access to an image in a private
-- Docker registry, set to @Vpc@.
newTrainingImageConfig ::
  -- | 'trainingRepositoryAccessMode'
  TrainingRepositoryAccessMode ->
  TrainingImageConfig
newTrainingImageConfig pTrainingRepositoryAccessMode_ =
  TrainingImageConfig'
    { trainingRepositoryAuthConfig =
        Prelude.Nothing,
      trainingRepositoryAccessMode =
        pTrainingRepositoryAccessMode_
    }

-- | An object containing authentication information for a private Docker
-- registry containing your training images.
trainingImageConfig_trainingRepositoryAuthConfig :: Lens.Lens' TrainingImageConfig (Prelude.Maybe TrainingRepositoryAuthConfig)
trainingImageConfig_trainingRepositoryAuthConfig = Lens.lens (\TrainingImageConfig' {trainingRepositoryAuthConfig} -> trainingRepositoryAuthConfig) (\s@TrainingImageConfig' {} a -> s {trainingRepositoryAuthConfig = a} :: TrainingImageConfig)

-- | The method that your training job will use to gain access to the images
-- in your private Docker registry. For access to an image in a private
-- Docker registry, set to @Vpc@.
trainingImageConfig_trainingRepositoryAccessMode :: Lens.Lens' TrainingImageConfig TrainingRepositoryAccessMode
trainingImageConfig_trainingRepositoryAccessMode = Lens.lens (\TrainingImageConfig' {trainingRepositoryAccessMode} -> trainingRepositoryAccessMode) (\s@TrainingImageConfig' {} a -> s {trainingRepositoryAccessMode = a} :: TrainingImageConfig)

instance Data.FromJSON TrainingImageConfig where
  parseJSON =
    Data.withObject
      "TrainingImageConfig"
      ( \x ->
          TrainingImageConfig'
            Prelude.<$> (x Data..:? "TrainingRepositoryAuthConfig")
            Prelude.<*> (x Data..: "TrainingRepositoryAccessMode")
      )

instance Prelude.Hashable TrainingImageConfig where
  hashWithSalt _salt TrainingImageConfig' {..} =
    _salt
      `Prelude.hashWithSalt` trainingRepositoryAuthConfig
      `Prelude.hashWithSalt` trainingRepositoryAccessMode

instance Prelude.NFData TrainingImageConfig where
  rnf TrainingImageConfig' {..} =
    Prelude.rnf trainingRepositoryAuthConfig
      `Prelude.seq` Prelude.rnf trainingRepositoryAccessMode

instance Data.ToJSON TrainingImageConfig where
  toJSON TrainingImageConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TrainingRepositoryAuthConfig" Data..=)
              Prelude.<$> trainingRepositoryAuthConfig,
            Prelude.Just
              ( "TrainingRepositoryAccessMode"
                  Data..= trainingRepositoryAccessMode
              )
          ]
      )
