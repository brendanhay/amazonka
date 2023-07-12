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
-- Module      : Amazonka.SageMaker.Types.ModelPackageValidationSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelPackageValidationSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelPackageValidationProfile

-- | Specifies batch transform jobs that SageMaker runs to validate your
-- model package.
--
-- /See:/ 'newModelPackageValidationSpecification' smart constructor.
data ModelPackageValidationSpecification = ModelPackageValidationSpecification'
  { -- | The IAM roles to be used for the validation of the model package.
    validationRole :: Prelude.Text,
    -- | An array of @ModelPackageValidationProfile@ objects, each of which
    -- specifies a batch transform job that SageMaker runs to validate your
    -- model package.
    validationProfiles :: Prelude.NonEmpty ModelPackageValidationProfile
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- specifies a batch transform job that SageMaker runs to validate your
-- model package.
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
          Lens.coerced
            Lens.# pValidationProfiles_
      }

-- | The IAM roles to be used for the validation of the model package.
modelPackageValidationSpecification_validationRole :: Lens.Lens' ModelPackageValidationSpecification Prelude.Text
modelPackageValidationSpecification_validationRole = Lens.lens (\ModelPackageValidationSpecification' {validationRole} -> validationRole) (\s@ModelPackageValidationSpecification' {} a -> s {validationRole = a} :: ModelPackageValidationSpecification)

-- | An array of @ModelPackageValidationProfile@ objects, each of which
-- specifies a batch transform job that SageMaker runs to validate your
-- model package.
modelPackageValidationSpecification_validationProfiles :: Lens.Lens' ModelPackageValidationSpecification (Prelude.NonEmpty ModelPackageValidationProfile)
modelPackageValidationSpecification_validationProfiles = Lens.lens (\ModelPackageValidationSpecification' {validationProfiles} -> validationProfiles) (\s@ModelPackageValidationSpecification' {} a -> s {validationProfiles = a} :: ModelPackageValidationSpecification) Prelude.. Lens.coerced

instance
  Data.FromJSON
    ModelPackageValidationSpecification
  where
  parseJSON =
    Data.withObject
      "ModelPackageValidationSpecification"
      ( \x ->
          ModelPackageValidationSpecification'
            Prelude.<$> (x Data..: "ValidationRole")
            Prelude.<*> (x Data..: "ValidationProfiles")
      )

instance
  Prelude.Hashable
    ModelPackageValidationSpecification
  where
  hashWithSalt
    _salt
    ModelPackageValidationSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` validationRole
        `Prelude.hashWithSalt` validationProfiles

instance
  Prelude.NFData
    ModelPackageValidationSpecification
  where
  rnf ModelPackageValidationSpecification' {..} =
    Prelude.rnf validationRole
      `Prelude.seq` Prelude.rnf validationProfiles

instance
  Data.ToJSON
    ModelPackageValidationSpecification
  where
  toJSON ModelPackageValidationSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ValidationRole" Data..= validationRole),
            Prelude.Just
              ("ValidationProfiles" Data..= validationProfiles)
          ]
      )
