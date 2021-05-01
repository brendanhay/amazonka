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
-- Module      : Network.AWS.Rekognition.Types.ValidationData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ValidationData where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.Asset

-- | Contains the Amazon S3 bucket location of the validation data for a
-- model training job.
--
-- The validation data includes error information for individual JSON lines
-- in the dataset. For more information, see Debugging a Failed Model
-- Training in the Amazon Rekognition Custom Labels Developer Guide.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
validationData_assets = Lens.lens (\ValidationData' {assets} -> assets) (\s@ValidationData' {} a -> s {assets = a} :: ValidationData) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON ValidationData where
  parseJSON =
    Prelude.withObject
      "ValidationData"
      ( \x ->
          ValidationData'
            Prelude.<$> (x Prelude..:? "Assets" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable ValidationData

instance Prelude.NFData ValidationData
