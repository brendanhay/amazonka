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
-- Module      : Network.AWS.SageMaker.Types.ModelPackageValidationProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageValidationProfile where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.TransformJobDefinition

-- | Contains data, such as the inputs and targeted instance types that are
-- used in the process of validating the model package.
--
-- The data provided in the validation profile is made available to your
-- buyers on AWS Marketplace.
--
-- /See:/ 'newModelPackageValidationProfile' smart constructor.
data ModelPackageValidationProfile = ModelPackageValidationProfile'
  { -- | The name of the profile for the model package.
    profileName :: Prelude.Text,
    -- | The @TransformJobDefinition@ object that describes the transform job
    -- used for the validation of the model package.
    transformJobDefinition :: TransformJobDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModelPackageValidationProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileName', 'modelPackageValidationProfile_profileName' - The name of the profile for the model package.
--
-- 'transformJobDefinition', 'modelPackageValidationProfile_transformJobDefinition' - The @TransformJobDefinition@ object that describes the transform job
-- used for the validation of the model package.
newModelPackageValidationProfile ::
  -- | 'profileName'
  Prelude.Text ->
  -- | 'transformJobDefinition'
  TransformJobDefinition ->
  ModelPackageValidationProfile
newModelPackageValidationProfile
  pProfileName_
  pTransformJobDefinition_ =
    ModelPackageValidationProfile'
      { profileName =
          pProfileName_,
        transformJobDefinition =
          pTransformJobDefinition_
      }

-- | The name of the profile for the model package.
modelPackageValidationProfile_profileName :: Lens.Lens' ModelPackageValidationProfile Prelude.Text
modelPackageValidationProfile_profileName = Lens.lens (\ModelPackageValidationProfile' {profileName} -> profileName) (\s@ModelPackageValidationProfile' {} a -> s {profileName = a} :: ModelPackageValidationProfile)

-- | The @TransformJobDefinition@ object that describes the transform job
-- used for the validation of the model package.
modelPackageValidationProfile_transformJobDefinition :: Lens.Lens' ModelPackageValidationProfile TransformJobDefinition
modelPackageValidationProfile_transformJobDefinition = Lens.lens (\ModelPackageValidationProfile' {transformJobDefinition} -> transformJobDefinition) (\s@ModelPackageValidationProfile' {} a -> s {transformJobDefinition = a} :: ModelPackageValidationProfile)

instance
  Prelude.FromJSON
    ModelPackageValidationProfile
  where
  parseJSON =
    Prelude.withObject
      "ModelPackageValidationProfile"
      ( \x ->
          ModelPackageValidationProfile'
            Prelude.<$> (x Prelude..: "ProfileName")
            Prelude.<*> (x Prelude..: "TransformJobDefinition")
      )

instance
  Prelude.Hashable
    ModelPackageValidationProfile

instance Prelude.NFData ModelPackageValidationProfile

instance Prelude.ToJSON ModelPackageValidationProfile where
  toJSON ModelPackageValidationProfile' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ProfileName" Prelude..= profileName),
            Prelude.Just
              ( "TransformJobDefinition"
                  Prelude..= transformJobDefinition
              )
          ]
      )
