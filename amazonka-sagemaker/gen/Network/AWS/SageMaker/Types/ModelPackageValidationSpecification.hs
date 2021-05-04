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
-- Module      : Network.AWS.SageMaker.Types.ModelPackageValidationSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageValidationSpecification where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.ModelPackageValidationProfile

-- | Specifies batch transform jobs that Amazon SageMaker runs to validate
-- your model package.
--
-- /See:/ 'newModelPackageValidationSpecification' smart constructor.
data ModelPackageValidationSpecification = ModelPackageValidationSpecification'
  { -- | The IAM roles to be used for the validation of the model package.
    validationRole :: Prelude.Text,
    -- | An array of @ModelPackageValidationProfile@ objects, each of which
    -- specifies a batch transform job that Amazon SageMaker runs to validate
    -- your model package.
    validationProfiles :: Prelude.NonEmpty ModelPackageValidationProfile
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'validationProfiles'
  Prelude.NonEmpty ModelPackageValidationProfile ->
  ModelPackageValidationSpecification
newModelPackageValidationSpecification
  pValidationRole_
  pValidationProfiles_ =
    ModelPackageValidationSpecification'
      { validationRole =
          pValidationRole_,
        validationProfiles =
          Prelude._Coerce
            Lens.# pValidationProfiles_
      }

-- | The IAM roles to be used for the validation of the model package.
modelPackageValidationSpecification_validationRole :: Lens.Lens' ModelPackageValidationSpecification Prelude.Text
modelPackageValidationSpecification_validationRole = Lens.lens (\ModelPackageValidationSpecification' {validationRole} -> validationRole) (\s@ModelPackageValidationSpecification' {} a -> s {validationRole = a} :: ModelPackageValidationSpecification)

-- | An array of @ModelPackageValidationProfile@ objects, each of which
-- specifies a batch transform job that Amazon SageMaker runs to validate
-- your model package.
modelPackageValidationSpecification_validationProfiles :: Lens.Lens' ModelPackageValidationSpecification (Prelude.NonEmpty ModelPackageValidationProfile)
modelPackageValidationSpecification_validationProfiles = Lens.lens (\ModelPackageValidationSpecification' {validationProfiles} -> validationProfiles) (\s@ModelPackageValidationSpecification' {} a -> s {validationProfiles = a} :: ModelPackageValidationSpecification) Prelude.. Prelude._Coerce

instance
  Prelude.FromJSON
    ModelPackageValidationSpecification
  where
  parseJSON =
    Prelude.withObject
      "ModelPackageValidationSpecification"
      ( \x ->
          ModelPackageValidationSpecification'
            Prelude.<$> (x Prelude..: "ValidationRole")
            Prelude.<*> (x Prelude..: "ValidationProfiles")
      )

instance
  Prelude.Hashable
    ModelPackageValidationSpecification

instance
  Prelude.NFData
    ModelPackageValidationSpecification

instance
  Prelude.ToJSON
    ModelPackageValidationSpecification
  where
  toJSON ModelPackageValidationSpecification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ValidationRole" Prelude..= validationRole),
            Prelude.Just
              ( "ValidationProfiles"
                  Prelude..= validationProfiles
              )
          ]
      )
