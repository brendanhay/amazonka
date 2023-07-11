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
-- Module      : Amazonka.Rekognition.Types.ValidationData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.ValidationData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.Asset

-- | Contains the Amazon S3 bucket location of the validation data for a
-- model training job.
--
-- The validation data includes error information for individual JSON Lines
-- in the dataset. For more information, see /Debugging a Failed Model
-- Training/ in the Amazon Rekognition Custom Labels Developer Guide.
--
-- You get the @ValidationData@ object for the training dataset
-- (TrainingDataResult) and the test dataset (TestingDataResult) by calling
-- DescribeProjectVersions.
--
-- The assets array contains a single Asset object. The GroundTruthManifest
-- field of the Asset object contains the S3 bucket location of the
-- validation data.
--
-- /See:/ 'newValidationData' smart constructor.
data ValidationData = ValidationData'
  { -- | The assets that comprise the validation data.
    assets :: Prelude.Maybe [Asset]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidationData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assets', 'validationData_assets' - The assets that comprise the validation data.
newValidationData ::
  ValidationData
newValidationData =
  ValidationData' {assets = Prelude.Nothing}

-- | The assets that comprise the validation data.
validationData_assets :: Lens.Lens' ValidationData (Prelude.Maybe [Asset])
validationData_assets = Lens.lens (\ValidationData' {assets} -> assets) (\s@ValidationData' {} a -> s {assets = a} :: ValidationData) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ValidationData where
  parseJSON =
    Data.withObject
      "ValidationData"
      ( \x ->
          ValidationData'
            Prelude.<$> (x Data..:? "Assets" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ValidationData where
  hashWithSalt _salt ValidationData' {..} =
    _salt `Prelude.hashWithSalt` assets

instance Prelude.NFData ValidationData where
  rnf ValidationData' {..} = Prelude.rnf assets
