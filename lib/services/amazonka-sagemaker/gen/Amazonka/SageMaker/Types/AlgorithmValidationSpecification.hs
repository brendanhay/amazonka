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
-- Module      : Amazonka.SageMaker.Types.AlgorithmValidationSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AlgorithmValidationSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AlgorithmValidationProfile

-- | Specifies configurations for one or more training jobs that SageMaker
-- runs to test the algorithm.
--
-- /See:/ 'newAlgorithmValidationSpecification' smart constructor.
data AlgorithmValidationSpecification = AlgorithmValidationSpecification'
  { -- | The IAM roles that SageMaker uses to run the training jobs.
    validationRole :: Prelude.Text,
    -- | An array of @AlgorithmValidationProfile@ objects, each of which
    -- specifies a training job and batch transform job that SageMaker runs to
    -- validate your algorithm.
    validationProfiles :: Prelude.NonEmpty AlgorithmValidationProfile
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlgorithmValidationSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validationRole', 'algorithmValidationSpecification_validationRole' - The IAM roles that SageMaker uses to run the training jobs.
--
-- 'validationProfiles', 'algorithmValidationSpecification_validationProfiles' - An array of @AlgorithmValidationProfile@ objects, each of which
-- specifies a training job and batch transform job that SageMaker runs to
-- validate your algorithm.
newAlgorithmValidationSpecification ::
  -- | 'validationRole'
  Prelude.Text ->
  -- | 'validationProfiles'
  Prelude.NonEmpty AlgorithmValidationProfile ->
  AlgorithmValidationSpecification
newAlgorithmValidationSpecification
  pValidationRole_
  pValidationProfiles_ =
    AlgorithmValidationSpecification'
      { validationRole =
          pValidationRole_,
        validationProfiles =
          Lens.coerced
            Lens.# pValidationProfiles_
      }

-- | The IAM roles that SageMaker uses to run the training jobs.
algorithmValidationSpecification_validationRole :: Lens.Lens' AlgorithmValidationSpecification Prelude.Text
algorithmValidationSpecification_validationRole = Lens.lens (\AlgorithmValidationSpecification' {validationRole} -> validationRole) (\s@AlgorithmValidationSpecification' {} a -> s {validationRole = a} :: AlgorithmValidationSpecification)

-- | An array of @AlgorithmValidationProfile@ objects, each of which
-- specifies a training job and batch transform job that SageMaker runs to
-- validate your algorithm.
algorithmValidationSpecification_validationProfiles :: Lens.Lens' AlgorithmValidationSpecification (Prelude.NonEmpty AlgorithmValidationProfile)
algorithmValidationSpecification_validationProfiles = Lens.lens (\AlgorithmValidationSpecification' {validationProfiles} -> validationProfiles) (\s@AlgorithmValidationSpecification' {} a -> s {validationProfiles = a} :: AlgorithmValidationSpecification) Prelude.. Lens.coerced

instance
  Core.FromJSON
    AlgorithmValidationSpecification
  where
  parseJSON =
    Core.withObject
      "AlgorithmValidationSpecification"
      ( \x ->
          AlgorithmValidationSpecification'
            Prelude.<$> (x Core..: "ValidationRole")
            Prelude.<*> (x Core..: "ValidationProfiles")
      )

instance
  Prelude.Hashable
    AlgorithmValidationSpecification
  where
  hashWithSalt
    _salt
    AlgorithmValidationSpecification' {..} =
      _salt `Prelude.hashWithSalt` validationRole
        `Prelude.hashWithSalt` validationProfiles

instance
  Prelude.NFData
    AlgorithmValidationSpecification
  where
  rnf AlgorithmValidationSpecification' {..} =
    Prelude.rnf validationRole
      `Prelude.seq` Prelude.rnf validationProfiles

instance Core.ToJSON AlgorithmValidationSpecification where
  toJSON AlgorithmValidationSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ValidationRole" Core..= validationRole),
            Prelude.Just
              ("ValidationProfiles" Core..= validationProfiles)
          ]
      )
