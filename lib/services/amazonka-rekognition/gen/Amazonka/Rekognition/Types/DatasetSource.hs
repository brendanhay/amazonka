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
-- Module      : Amazonka.Rekognition.Types.DatasetSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DatasetSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.GroundTruthManifest

-- | The source that Amazon Rekognition Custom Labels uses to create a
-- dataset. To use an Amazon Sagemaker format manifest file, specify the S3
-- bucket location in the @GroundTruthManifest@ field. The S3 bucket must
-- be in your AWS account. To create a copy of an existing dataset, specify
-- the Amazon Resource Name (ARN) of an existing dataset in @DatasetArn@.
--
-- You need to specify a value for @DatasetArn@ or @GroundTruthManifest@,
-- but not both. if you supply both values, or if you don\'t specify any
-- values, an InvalidParameterException exception occurs.
--
-- For more information, see CreateDataset.
--
-- /See:/ 'newDatasetSource' smart constructor.
data DatasetSource = DatasetSource'
  { -- | The ARN of an Amazon Rekognition Custom Labels dataset that you want to
    -- copy.
    datasetArn :: Prelude.Maybe Prelude.Text,
    groundTruthManifest :: Prelude.Maybe GroundTruthManifest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetArn', 'datasetSource_datasetArn' - The ARN of an Amazon Rekognition Custom Labels dataset that you want to
-- copy.
--
-- 'groundTruthManifest', 'datasetSource_groundTruthManifest' - Undocumented member.
newDatasetSource ::
  DatasetSource
newDatasetSource =
  DatasetSource'
    { datasetArn = Prelude.Nothing,
      groundTruthManifest = Prelude.Nothing
    }

-- | The ARN of an Amazon Rekognition Custom Labels dataset that you want to
-- copy.
datasetSource_datasetArn :: Lens.Lens' DatasetSource (Prelude.Maybe Prelude.Text)
datasetSource_datasetArn = Lens.lens (\DatasetSource' {datasetArn} -> datasetArn) (\s@DatasetSource' {} a -> s {datasetArn = a} :: DatasetSource)

-- | Undocumented member.
datasetSource_groundTruthManifest :: Lens.Lens' DatasetSource (Prelude.Maybe GroundTruthManifest)
datasetSource_groundTruthManifest = Lens.lens (\DatasetSource' {groundTruthManifest} -> groundTruthManifest) (\s@DatasetSource' {} a -> s {groundTruthManifest = a} :: DatasetSource)

instance Prelude.Hashable DatasetSource where
  hashWithSalt _salt DatasetSource' {..} =
    _salt `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` groundTruthManifest

instance Prelude.NFData DatasetSource where
  rnf DatasetSource' {..} =
    Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf groundTruthManifest

instance Core.ToJSON DatasetSource where
  toJSON DatasetSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DatasetArn" Core..=) Prelude.<$> datasetArn,
            ("GroundTruthManifest" Core..=)
              Prelude.<$> groundTruthManifest
          ]
      )
