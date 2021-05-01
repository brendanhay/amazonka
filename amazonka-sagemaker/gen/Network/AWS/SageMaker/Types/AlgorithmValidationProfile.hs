{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.AlgorithmValidationProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmValidationProfile where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.TrainingJobDefinition
import Network.AWS.SageMaker.Types.TransformJobDefinition

-- | Defines a training job and a batch transform job that Amazon SageMaker
-- runs to validate your algorithm.
--
-- The data provided in the validation profile is made available to your
-- buyers on AWS Marketplace.
--
-- /See:/ 'newAlgorithmValidationProfile' smart constructor.
data AlgorithmValidationProfile = AlgorithmValidationProfile'
  { -- | The @TransformJobDefinition@ object that describes the transform job
    -- that Amazon SageMaker runs to validate your algorithm.
    transformJobDefinition :: Prelude.Maybe TransformJobDefinition,
    -- | The name of the profile for the algorithm. The name must have 1 to 63
    -- characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
    profileName :: Prelude.Text,
    -- | The @TrainingJobDefinition@ object that describes the training job that
    -- Amazon SageMaker runs to validate your algorithm.
    trainingJobDefinition :: TrainingJobDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AlgorithmValidationProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformJobDefinition', 'algorithmValidationProfile_transformJobDefinition' - The @TransformJobDefinition@ object that describes the transform job
-- that Amazon SageMaker runs to validate your algorithm.
--
-- 'profileName', 'algorithmValidationProfile_profileName' - The name of the profile for the algorithm. The name must have 1 to 63
-- characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- 'trainingJobDefinition', 'algorithmValidationProfile_trainingJobDefinition' - The @TrainingJobDefinition@ object that describes the training job that
-- Amazon SageMaker runs to validate your algorithm.
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
-- that Amazon SageMaker runs to validate your algorithm.
algorithmValidationProfile_transformJobDefinition :: Lens.Lens' AlgorithmValidationProfile (Prelude.Maybe TransformJobDefinition)
algorithmValidationProfile_transformJobDefinition = Lens.lens (\AlgorithmValidationProfile' {transformJobDefinition} -> transformJobDefinition) (\s@AlgorithmValidationProfile' {} a -> s {transformJobDefinition = a} :: AlgorithmValidationProfile)

-- | The name of the profile for the algorithm. The name must have 1 to 63
-- characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
algorithmValidationProfile_profileName :: Lens.Lens' AlgorithmValidationProfile Prelude.Text
algorithmValidationProfile_profileName = Lens.lens (\AlgorithmValidationProfile' {profileName} -> profileName) (\s@AlgorithmValidationProfile' {} a -> s {profileName = a} :: AlgorithmValidationProfile)

-- | The @TrainingJobDefinition@ object that describes the training job that
-- Amazon SageMaker runs to validate your algorithm.
algorithmValidationProfile_trainingJobDefinition :: Lens.Lens' AlgorithmValidationProfile TrainingJobDefinition
algorithmValidationProfile_trainingJobDefinition = Lens.lens (\AlgorithmValidationProfile' {trainingJobDefinition} -> trainingJobDefinition) (\s@AlgorithmValidationProfile' {} a -> s {trainingJobDefinition = a} :: AlgorithmValidationProfile)

instance Prelude.FromJSON AlgorithmValidationProfile where
  parseJSON =
    Prelude.withObject
      "AlgorithmValidationProfile"
      ( \x ->
          AlgorithmValidationProfile'
            Prelude.<$> (x Prelude..:? "TransformJobDefinition")
            Prelude.<*> (x Prelude..: "ProfileName")
            Prelude.<*> (x Prelude..: "TrainingJobDefinition")
      )

instance Prelude.Hashable AlgorithmValidationProfile

instance Prelude.NFData AlgorithmValidationProfile

instance Prelude.ToJSON AlgorithmValidationProfile where
  toJSON AlgorithmValidationProfile' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("TransformJobDefinition" Prelude..=)
              Prelude.<$> transformJobDefinition,
            Prelude.Just ("ProfileName" Prelude..= profileName),
            Prelude.Just
              ( "TrainingJobDefinition"
                  Prelude..= trainingJobDefinition
              )
          ]
      )
