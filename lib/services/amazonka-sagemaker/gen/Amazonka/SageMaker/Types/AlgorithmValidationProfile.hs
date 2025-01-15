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
-- Module      : Amazonka.SageMaker.Types.AlgorithmValidationProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AlgorithmValidationProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.TrainingJobDefinition
import Amazonka.SageMaker.Types.TransformJobDefinition

-- | Defines a training job and a batch transform job that SageMaker runs to
-- validate your algorithm.
--
-- The data provided in the validation profile is made available to your
-- buyers on Amazon Web Services Marketplace.
--
-- /See:/ 'newAlgorithmValidationProfile' smart constructor.
data AlgorithmValidationProfile = AlgorithmValidationProfile'
  { -- | The @TransformJobDefinition@ object that describes the transform job
    -- that SageMaker runs to validate your algorithm.
    transformJobDefinition :: Prelude.Maybe TransformJobDefinition,
    -- | The name of the profile for the algorithm. The name must have 1 to 63
    -- characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
    profileName :: Prelude.Text,
    -- | The @TrainingJobDefinition@ object that describes the training job that
    -- SageMaker runs to validate your algorithm.
    trainingJobDefinition :: TrainingJobDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlgorithmValidationProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformJobDefinition', 'algorithmValidationProfile_transformJobDefinition' - The @TransformJobDefinition@ object that describes the transform job
-- that SageMaker runs to validate your algorithm.
--
-- 'profileName', 'algorithmValidationProfile_profileName' - The name of the profile for the algorithm. The name must have 1 to 63
-- characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- 'trainingJobDefinition', 'algorithmValidationProfile_trainingJobDefinition' - The @TrainingJobDefinition@ object that describes the training job that
-- SageMaker runs to validate your algorithm.
newAlgorithmValidationProfile ::
  -- | 'profileName'
  Prelude.Text ->
  -- | 'trainingJobDefinition'
  TrainingJobDefinition ->
  AlgorithmValidationProfile
newAlgorithmValidationProfile
  pProfileName_
  pTrainingJobDefinition_ =
    AlgorithmValidationProfile'
      { transformJobDefinition =
          Prelude.Nothing,
        profileName = pProfileName_,
        trainingJobDefinition = pTrainingJobDefinition_
      }

-- | The @TransformJobDefinition@ object that describes the transform job
-- that SageMaker runs to validate your algorithm.
algorithmValidationProfile_transformJobDefinition :: Lens.Lens' AlgorithmValidationProfile (Prelude.Maybe TransformJobDefinition)
algorithmValidationProfile_transformJobDefinition = Lens.lens (\AlgorithmValidationProfile' {transformJobDefinition} -> transformJobDefinition) (\s@AlgorithmValidationProfile' {} a -> s {transformJobDefinition = a} :: AlgorithmValidationProfile)

-- | The name of the profile for the algorithm. The name must have 1 to 63
-- characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
algorithmValidationProfile_profileName :: Lens.Lens' AlgorithmValidationProfile Prelude.Text
algorithmValidationProfile_profileName = Lens.lens (\AlgorithmValidationProfile' {profileName} -> profileName) (\s@AlgorithmValidationProfile' {} a -> s {profileName = a} :: AlgorithmValidationProfile)

-- | The @TrainingJobDefinition@ object that describes the training job that
-- SageMaker runs to validate your algorithm.
algorithmValidationProfile_trainingJobDefinition :: Lens.Lens' AlgorithmValidationProfile TrainingJobDefinition
algorithmValidationProfile_trainingJobDefinition = Lens.lens (\AlgorithmValidationProfile' {trainingJobDefinition} -> trainingJobDefinition) (\s@AlgorithmValidationProfile' {} a -> s {trainingJobDefinition = a} :: AlgorithmValidationProfile)

instance Data.FromJSON AlgorithmValidationProfile where
  parseJSON =
    Data.withObject
      "AlgorithmValidationProfile"
      ( \x ->
          AlgorithmValidationProfile'
            Prelude.<$> (x Data..:? "TransformJobDefinition")
            Prelude.<*> (x Data..: "ProfileName")
            Prelude.<*> (x Data..: "TrainingJobDefinition")
      )

instance Prelude.Hashable AlgorithmValidationProfile where
  hashWithSalt _salt AlgorithmValidationProfile' {..} =
    _salt
      `Prelude.hashWithSalt` transformJobDefinition
      `Prelude.hashWithSalt` profileName
      `Prelude.hashWithSalt` trainingJobDefinition

instance Prelude.NFData AlgorithmValidationProfile where
  rnf AlgorithmValidationProfile' {..} =
    Prelude.rnf transformJobDefinition `Prelude.seq`
      Prelude.rnf profileName `Prelude.seq`
        Prelude.rnf trainingJobDefinition

instance Data.ToJSON AlgorithmValidationProfile where
  toJSON AlgorithmValidationProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TransformJobDefinition" Data..=)
              Prelude.<$> transformJobDefinition,
            Prelude.Just ("ProfileName" Data..= profileName),
            Prelude.Just
              ( "TrainingJobDefinition"
                  Data..= trainingJobDefinition
              )
          ]
      )
