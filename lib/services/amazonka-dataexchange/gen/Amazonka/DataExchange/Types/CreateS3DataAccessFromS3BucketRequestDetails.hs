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
-- Module      : Amazonka.DataExchange.Types.CreateS3DataAccessFromS3BucketRequestDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.CreateS3DataAccessFromS3BucketRequestDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.S3DataAccessAssetSourceEntry
import qualified Amazonka.Prelude as Prelude

-- | Details of the operation to create an Amazon S3 data access from an S3
-- bucket.
--
-- /See:/ 'newCreateS3DataAccessFromS3BucketRequestDetails' smart constructor.
data CreateS3DataAccessFromS3BucketRequestDetails = CreateS3DataAccessFromS3BucketRequestDetails'
  { -- | Details about the S3 data access source asset.
    assetSource :: S3DataAccessAssetSourceEntry,
    -- | The unique identifier for the data set associated with the creation of
    -- this Amazon S3 data access.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for a revision.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateS3DataAccessFromS3BucketRequestDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetSource', 'createS3DataAccessFromS3BucketRequestDetails_assetSource' - Details about the S3 data access source asset.
--
-- 'dataSetId', 'createS3DataAccessFromS3BucketRequestDetails_dataSetId' - The unique identifier for the data set associated with the creation of
-- this Amazon S3 data access.
--
-- 'revisionId', 'createS3DataAccessFromS3BucketRequestDetails_revisionId' - The unique identifier for a revision.
newCreateS3DataAccessFromS3BucketRequestDetails ::
  -- | 'assetSource'
  S3DataAccessAssetSourceEntry ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  CreateS3DataAccessFromS3BucketRequestDetails
newCreateS3DataAccessFromS3BucketRequestDetails
  pAssetSource_
  pDataSetId_
  pRevisionId_ =
    CreateS3DataAccessFromS3BucketRequestDetails'
      { assetSource =
          pAssetSource_,
        dataSetId = pDataSetId_,
        revisionId = pRevisionId_
      }

-- | Details about the S3 data access source asset.
createS3DataAccessFromS3BucketRequestDetails_assetSource :: Lens.Lens' CreateS3DataAccessFromS3BucketRequestDetails S3DataAccessAssetSourceEntry
createS3DataAccessFromS3BucketRequestDetails_assetSource = Lens.lens (\CreateS3DataAccessFromS3BucketRequestDetails' {assetSource} -> assetSource) (\s@CreateS3DataAccessFromS3BucketRequestDetails' {} a -> s {assetSource = a} :: CreateS3DataAccessFromS3BucketRequestDetails)

-- | The unique identifier for the data set associated with the creation of
-- this Amazon S3 data access.
createS3DataAccessFromS3BucketRequestDetails_dataSetId :: Lens.Lens' CreateS3DataAccessFromS3BucketRequestDetails Prelude.Text
createS3DataAccessFromS3BucketRequestDetails_dataSetId = Lens.lens (\CreateS3DataAccessFromS3BucketRequestDetails' {dataSetId} -> dataSetId) (\s@CreateS3DataAccessFromS3BucketRequestDetails' {} a -> s {dataSetId = a} :: CreateS3DataAccessFromS3BucketRequestDetails)

-- | The unique identifier for a revision.
createS3DataAccessFromS3BucketRequestDetails_revisionId :: Lens.Lens' CreateS3DataAccessFromS3BucketRequestDetails Prelude.Text
createS3DataAccessFromS3BucketRequestDetails_revisionId = Lens.lens (\CreateS3DataAccessFromS3BucketRequestDetails' {revisionId} -> revisionId) (\s@CreateS3DataAccessFromS3BucketRequestDetails' {} a -> s {revisionId = a} :: CreateS3DataAccessFromS3BucketRequestDetails)

instance
  Prelude.Hashable
    CreateS3DataAccessFromS3BucketRequestDetails
  where
  hashWithSalt
    _salt
    CreateS3DataAccessFromS3BucketRequestDetails' {..} =
      _salt
        `Prelude.hashWithSalt` assetSource
        `Prelude.hashWithSalt` dataSetId
        `Prelude.hashWithSalt` revisionId

instance
  Prelude.NFData
    CreateS3DataAccessFromS3BucketRequestDetails
  where
  rnf CreateS3DataAccessFromS3BucketRequestDetails' {..} =
    Prelude.rnf assetSource `Prelude.seq`
      Prelude.rnf dataSetId `Prelude.seq`
        Prelude.rnf revisionId

instance
  Data.ToJSON
    CreateS3DataAccessFromS3BucketRequestDetails
  where
  toJSON
    CreateS3DataAccessFromS3BucketRequestDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ Prelude.Just ("AssetSource" Data..= assetSource),
              Prelude.Just ("DataSetId" Data..= dataSetId),
              Prelude.Just ("RevisionId" Data..= revisionId)
            ]
        )
