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
-- Module      : Amazonka.SageMaker.Types.PipelineDefinitionS3Location
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.PipelineDefinitionS3Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The location of the pipeline definition stored in Amazon S3.
--
-- /See:/ 'newPipelineDefinitionS3Location' smart constructor.
data PipelineDefinitionS3Location = PipelineDefinitionS3Location'
  { -- | Version Id of the pipeline definition file. If not specified, Amazon
    -- SageMaker will retrieve the latest version.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | Name of the S3 bucket.
    bucket :: Prelude.Text,
    -- | The object key (or key name) uniquely identifies the object in an S3
    -- bucket.
    objectKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineDefinitionS3Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'pipelineDefinitionS3Location_versionId' - Version Id of the pipeline definition file. If not specified, Amazon
-- SageMaker will retrieve the latest version.
--
-- 'bucket', 'pipelineDefinitionS3Location_bucket' - Name of the S3 bucket.
--
-- 'objectKey', 'pipelineDefinitionS3Location_objectKey' - The object key (or key name) uniquely identifies the object in an S3
-- bucket.
newPipelineDefinitionS3Location ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'objectKey'
  Prelude.Text ->
  PipelineDefinitionS3Location
newPipelineDefinitionS3Location pBucket_ pObjectKey_ =
  PipelineDefinitionS3Location'
    { versionId =
        Prelude.Nothing,
      bucket = pBucket_,
      objectKey = pObjectKey_
    }

-- | Version Id of the pipeline definition file. If not specified, Amazon
-- SageMaker will retrieve the latest version.
pipelineDefinitionS3Location_versionId :: Lens.Lens' PipelineDefinitionS3Location (Prelude.Maybe Prelude.Text)
pipelineDefinitionS3Location_versionId = Lens.lens (\PipelineDefinitionS3Location' {versionId} -> versionId) (\s@PipelineDefinitionS3Location' {} a -> s {versionId = a} :: PipelineDefinitionS3Location)

-- | Name of the S3 bucket.
pipelineDefinitionS3Location_bucket :: Lens.Lens' PipelineDefinitionS3Location Prelude.Text
pipelineDefinitionS3Location_bucket = Lens.lens (\PipelineDefinitionS3Location' {bucket} -> bucket) (\s@PipelineDefinitionS3Location' {} a -> s {bucket = a} :: PipelineDefinitionS3Location)

-- | The object key (or key name) uniquely identifies the object in an S3
-- bucket.
pipelineDefinitionS3Location_objectKey :: Lens.Lens' PipelineDefinitionS3Location Prelude.Text
pipelineDefinitionS3Location_objectKey = Lens.lens (\PipelineDefinitionS3Location' {objectKey} -> objectKey) (\s@PipelineDefinitionS3Location' {} a -> s {objectKey = a} :: PipelineDefinitionS3Location)

instance
  Prelude.Hashable
    PipelineDefinitionS3Location
  where
  hashWithSalt _salt PipelineDefinitionS3Location' {..} =
    _salt `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` objectKey

instance Prelude.NFData PipelineDefinitionS3Location where
  rnf PipelineDefinitionS3Location' {..} =
    Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf objectKey

instance Data.ToJSON PipelineDefinitionS3Location where
  toJSON PipelineDefinitionS3Location' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VersionId" Data..=) Prelude.<$> versionId,
            Prelude.Just ("Bucket" Data..= bucket),
            Prelude.Just ("ObjectKey" Data..= objectKey)
          ]
      )
