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
-- Module      : Amazonka.DataExchange.Types.ImportAssetFromSignedUrlRequestDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ImportAssetFromSignedUrlRequestDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of the operation to be performed by the job.
--
-- /See:/ 'newImportAssetFromSignedUrlRequestDetails' smart constructor.
data ImportAssetFromSignedUrlRequestDetails = ImportAssetFromSignedUrlRequestDetails'
  { -- | The name of the asset. When importing from Amazon S3, the Amazon S3
    -- object key is used as the asset name.
    assetName :: Prelude.Text,
    -- | The unique identifier for the data set associated with this import job.
    dataSetId :: Prelude.Text,
    -- | The Base64-encoded Md5 hash for the asset, used to ensure the integrity
    -- of the file at that location.
    md5Hash :: Prelude.Text,
    -- | The unique identifier for the revision associated with this import
    -- request.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportAssetFromSignedUrlRequestDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetName', 'importAssetFromSignedUrlRequestDetails_assetName' - The name of the asset. When importing from Amazon S3, the Amazon S3
-- object key is used as the asset name.
--
-- 'dataSetId', 'importAssetFromSignedUrlRequestDetails_dataSetId' - The unique identifier for the data set associated with this import job.
--
-- 'md5Hash', 'importAssetFromSignedUrlRequestDetails_md5Hash' - The Base64-encoded Md5 hash for the asset, used to ensure the integrity
-- of the file at that location.
--
-- 'revisionId', 'importAssetFromSignedUrlRequestDetails_revisionId' - The unique identifier for the revision associated with this import
-- request.
newImportAssetFromSignedUrlRequestDetails ::
  -- | 'assetName'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'md5Hash'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  ImportAssetFromSignedUrlRequestDetails
newImportAssetFromSignedUrlRequestDetails
  pAssetName_
  pDataSetId_
  pMd5Hash_
  pRevisionId_ =
    ImportAssetFromSignedUrlRequestDetails'
      { assetName =
          pAssetName_,
        dataSetId = pDataSetId_,
        md5Hash = pMd5Hash_,
        revisionId = pRevisionId_
      }

-- | The name of the asset. When importing from Amazon S3, the Amazon S3
-- object key is used as the asset name.
importAssetFromSignedUrlRequestDetails_assetName :: Lens.Lens' ImportAssetFromSignedUrlRequestDetails Prelude.Text
importAssetFromSignedUrlRequestDetails_assetName = Lens.lens (\ImportAssetFromSignedUrlRequestDetails' {assetName} -> assetName) (\s@ImportAssetFromSignedUrlRequestDetails' {} a -> s {assetName = a} :: ImportAssetFromSignedUrlRequestDetails)

-- | The unique identifier for the data set associated with this import job.
importAssetFromSignedUrlRequestDetails_dataSetId :: Lens.Lens' ImportAssetFromSignedUrlRequestDetails Prelude.Text
importAssetFromSignedUrlRequestDetails_dataSetId = Lens.lens (\ImportAssetFromSignedUrlRequestDetails' {dataSetId} -> dataSetId) (\s@ImportAssetFromSignedUrlRequestDetails' {} a -> s {dataSetId = a} :: ImportAssetFromSignedUrlRequestDetails)

-- | The Base64-encoded Md5 hash for the asset, used to ensure the integrity
-- of the file at that location.
importAssetFromSignedUrlRequestDetails_md5Hash :: Lens.Lens' ImportAssetFromSignedUrlRequestDetails Prelude.Text
importAssetFromSignedUrlRequestDetails_md5Hash = Lens.lens (\ImportAssetFromSignedUrlRequestDetails' {md5Hash} -> md5Hash) (\s@ImportAssetFromSignedUrlRequestDetails' {} a -> s {md5Hash = a} :: ImportAssetFromSignedUrlRequestDetails)

-- | The unique identifier for the revision associated with this import
-- request.
importAssetFromSignedUrlRequestDetails_revisionId :: Lens.Lens' ImportAssetFromSignedUrlRequestDetails Prelude.Text
importAssetFromSignedUrlRequestDetails_revisionId = Lens.lens (\ImportAssetFromSignedUrlRequestDetails' {revisionId} -> revisionId) (\s@ImportAssetFromSignedUrlRequestDetails' {} a -> s {revisionId = a} :: ImportAssetFromSignedUrlRequestDetails)

instance
  Prelude.Hashable
    ImportAssetFromSignedUrlRequestDetails
  where
  hashWithSalt
    _salt
    ImportAssetFromSignedUrlRequestDetails' {..} =
      _salt
        `Prelude.hashWithSalt` assetName
        `Prelude.hashWithSalt` dataSetId
        `Prelude.hashWithSalt` md5Hash
        `Prelude.hashWithSalt` revisionId

instance
  Prelude.NFData
    ImportAssetFromSignedUrlRequestDetails
  where
  rnf ImportAssetFromSignedUrlRequestDetails' {..} =
    Prelude.rnf assetName
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf md5Hash
      `Prelude.seq` Prelude.rnf revisionId

instance
  Data.ToJSON
    ImportAssetFromSignedUrlRequestDetails
  where
  toJSON ImportAssetFromSignedUrlRequestDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AssetName" Data..= assetName),
            Prelude.Just ("DataSetId" Data..= dataSetId),
            Prelude.Just ("Md5Hash" Data..= md5Hash),
            Prelude.Just ("RevisionId" Data..= revisionId)
          ]
      )
