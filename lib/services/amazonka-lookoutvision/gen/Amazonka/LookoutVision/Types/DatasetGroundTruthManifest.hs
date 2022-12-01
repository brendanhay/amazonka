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
-- Module      : Amazonka.LookoutVision.Types.DatasetGroundTruthManifest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.DatasetGroundTruthManifest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutVision.Types.InputS3Object
import qualified Amazonka.Prelude as Prelude

-- | Location information about a manifest file. You can use a manifest file
-- to create a dataset.
--
-- /See:/ 'newDatasetGroundTruthManifest' smart constructor.
data DatasetGroundTruthManifest = DatasetGroundTruthManifest'
  { -- | The S3 bucket location for the manifest file.
    s3Object :: Prelude.Maybe InputS3Object
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetGroundTruthManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Object', 'datasetGroundTruthManifest_s3Object' - The S3 bucket location for the manifest file.
newDatasetGroundTruthManifest ::
  DatasetGroundTruthManifest
newDatasetGroundTruthManifest =
  DatasetGroundTruthManifest'
    { s3Object =
        Prelude.Nothing
    }

-- | The S3 bucket location for the manifest file.
datasetGroundTruthManifest_s3Object :: Lens.Lens' DatasetGroundTruthManifest (Prelude.Maybe InputS3Object)
datasetGroundTruthManifest_s3Object = Lens.lens (\DatasetGroundTruthManifest' {s3Object} -> s3Object) (\s@DatasetGroundTruthManifest' {} a -> s {s3Object = a} :: DatasetGroundTruthManifest)

instance Prelude.Hashable DatasetGroundTruthManifest where
  hashWithSalt _salt DatasetGroundTruthManifest' {..} =
    _salt `Prelude.hashWithSalt` s3Object

instance Prelude.NFData DatasetGroundTruthManifest where
  rnf DatasetGroundTruthManifest' {..} =
    Prelude.rnf s3Object

instance Core.ToJSON DatasetGroundTruthManifest where
  toJSON DatasetGroundTruthManifest' {..} =
    Core.object
      ( Prelude.catMaybes
          [("S3Object" Core..=) Prelude.<$> s3Object]
      )
