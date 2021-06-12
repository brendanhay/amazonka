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
-- Module      : Network.AWS.SageMaker.Types.ModelPackageValidationSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageValidationSpecification where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ModelPackageValidationProfile

-- | Specifies batch transform jobs that Amazon SageMaker runs to validate
-- your model package.
--
-- /See:/ 'newModelPackageValidationSpecification' smart constructor.
data ModelPackageValidationSpecification = ModelPackageValidationSpecification'
  { -- | The IAM roles to be used for the validation of the model package.
    validationRole :: Core.Text,
    -- | An array of @ModelPackageValidationProfile@ objects, each of which
    -- specifies a batch transform job that Amazon SageMaker runs to validate
    -- your model package.
    validationProfiles :: Core.NonEmpty ModelPackageValidationProfile
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModelPackageValidationSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validationRole', 'modelPackageValidationSpecification_validationRole' - The IAM roles to be used for the validation of the model package.
--
-- 'validationProfiles', 'modelPackageValidationSpecification_validationProfiles' - An array of @ModelPackageValidationProfile@ objects, each of which
-- specifies a batch transform job that Amazon SageMaker runs to validate
-- your model package.
newModelPackageValidationSpecification ::
  -- | 'validationRole'
  Core.Text ->
  -- | 'validationProfiles'
  Core.NonEmpty ModelPackageValidationProfile ->
  ModelPackageValidationSpecification
newModelPackageValidationSpecification
  pValidationRole_
  pValidationProfiles_ =
    ModelPackageValidationSpecification'
      { validationRole =
          pValidationRole_,
        validationProfiles =
          Lens._Coerce
            Lens.# pValidationProfiles_
      }

-- | The IAM roles to be used for the validation of the model package.
modelPackageValidationSpecification_validationRole :: Lens.Lens' ModelPackageValidationSpecification Core.Text
modelPackageValidationSpecification_validationRole = Lens.lens (\ModelPackageValidationSpecification' {validationRole} -> validationRole) (\s@ModelPackageValidationSpecification' {} a -> s {validationRole = a} :: ModelPackageValidationSpecification)

-- | An array of @ModelPackageValidationProfile@ objects, each of which
-- specifies a batch transform job that Amazon SageMaker runs to validate
-- your model package.
modelPackageValidationSpecification_validationProfiles :: Lens.Lens' ModelPackageValidationSpecification (Core.NonEmpty ModelPackageValidationProfile)
modelPackageValidationSpecification_validationProfiles = Lens.lens (\ModelPackageValidationSpecification' {validationProfiles} -> validationProfiles) (\s@ModelPackageValidationSpecification' {} a -> s {validationProfiles = a} :: ModelPackageValidationSpecification) Core.. Lens._Coerce

instance
  Core.FromJSON
    ModelPackageValidationSpecification
  where
  parseJSON =
    Core.withObject
      "ModelPackageValidationSpecification"
      ( \x ->
          ModelPackageValidationSpecification'
            Core.<$> (x Core..: "ValidationRole")
            Core.<*> (x Core..: "ValidationProfiles")
      )

instance
  Core.Hashable
    ModelPackageValidationSpecification

instance
  Core.NFData
    ModelPackageValidationSpecification

instance
  Core.ToJSON
    ModelPackageValidationSpecification
  where
  toJSON ModelPackageValidationSpecification' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ValidationRole" Core..= validationRole),
            Core.Just
              ("ValidationProfiles" Core..= validationProfiles)
          ]
      )
