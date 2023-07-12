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
-- Module      : Amazonka.DataExchange.Types.CreateS3DataAccessFromS3BucketResponseDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.CreateS3DataAccessFromS3BucketResponseDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.S3DataAccessAssetSourceEntry
import qualified Amazonka.Prelude as Prelude

-- | Details about the response of the operation to create an S3 data access
-- from an S3 bucket.
--
-- /See:/ 'newCreateS3DataAccessFromS3BucketResponseDetails' smart constructor.
data CreateS3DataAccessFromS3BucketResponseDetails = CreateS3DataAccessFromS3BucketResponseDetails'
  { -- | Details about the asset source from an Amazon S3 bucket.
    assetSource :: S3DataAccessAssetSourceEntry,
    -- | The unique identifier for this data set.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for the revision.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateS3DataAccessFromS3BucketResponseDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetSource', 'createS3DataAccessFromS3BucketResponseDetails_assetSource' - Details about the asset source from an Amazon S3 bucket.
--
-- 'dataSetId', 'createS3DataAccessFromS3BucketResponseDetails_dataSetId' - The unique identifier for this data set.
--
-- 'revisionId', 'createS3DataAccessFromS3BucketResponseDetails_revisionId' - The unique identifier for the revision.
newCreateS3DataAccessFromS3BucketResponseDetails ::
  -- | 'assetSource'
  S3DataAccessAssetSourceEntry ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  CreateS3DataAccessFromS3BucketResponseDetails
newCreateS3DataAccessFromS3BucketResponseDetails
  pAssetSource_
  pDataSetId_
  pRevisionId_ =
    CreateS3DataAccessFromS3BucketResponseDetails'
      { assetSource =
          pAssetSource_,
        dataSetId = pDataSetId_,
        revisionId = pRevisionId_
      }

-- | Details about the asset source from an Amazon S3 bucket.
createS3DataAccessFromS3BucketResponseDetails_assetSource :: Lens.Lens' CreateS3DataAccessFromS3BucketResponseDetails S3DataAccessAssetSourceEntry
createS3DataAccessFromS3BucketResponseDetails_assetSource = Lens.lens (\CreateS3DataAccessFromS3BucketResponseDetails' {assetSource} -> assetSource) (\s@CreateS3DataAccessFromS3BucketResponseDetails' {} a -> s {assetSource = a} :: CreateS3DataAccessFromS3BucketResponseDetails)

-- | The unique identifier for this data set.
createS3DataAccessFromS3BucketResponseDetails_dataSetId :: Lens.Lens' CreateS3DataAccessFromS3BucketResponseDetails Prelude.Text
createS3DataAccessFromS3BucketResponseDetails_dataSetId = Lens.lens (\CreateS3DataAccessFromS3BucketResponseDetails' {dataSetId} -> dataSetId) (\s@CreateS3DataAccessFromS3BucketResponseDetails' {} a -> s {dataSetId = a} :: CreateS3DataAccessFromS3BucketResponseDetails)

-- | The unique identifier for the revision.
createS3DataAccessFromS3BucketResponseDetails_revisionId :: Lens.Lens' CreateS3DataAccessFromS3BucketResponseDetails Prelude.Text
createS3DataAccessFromS3BucketResponseDetails_revisionId = Lens.lens (\CreateS3DataAccessFromS3BucketResponseDetails' {revisionId} -> revisionId) (\s@CreateS3DataAccessFromS3BucketResponseDetails' {} a -> s {revisionId = a} :: CreateS3DataAccessFromS3BucketResponseDetails)

instance
  Data.FromJSON
    CreateS3DataAccessFromS3BucketResponseDetails
  where
  parseJSON =
    Data.withObject
      "CreateS3DataAccessFromS3BucketResponseDetails"
      ( \x ->
          CreateS3DataAccessFromS3BucketResponseDetails'
            Prelude.<$> (x Data..: "AssetSource")
            Prelude.<*> (x Data..: "DataSetId")
            Prelude.<*> (x Data..: "RevisionId")
      )

instance
  Prelude.Hashable
    CreateS3DataAccessFromS3BucketResponseDetails
  where
  hashWithSalt
    _salt
    CreateS3DataAccessFromS3BucketResponseDetails' {..} =
      _salt
        `Prelude.hashWithSalt` assetSource
        `Prelude.hashWithSalt` dataSetId
        `Prelude.hashWithSalt` revisionId

instance
  Prelude.NFData
    CreateS3DataAccessFromS3BucketResponseDetails
  where
  rnf
    CreateS3DataAccessFromS3BucketResponseDetails' {..} =
      Prelude.rnf assetSource
        `Prelude.seq` Prelude.rnf dataSetId
        `Prelude.seq` Prelude.rnf revisionId
