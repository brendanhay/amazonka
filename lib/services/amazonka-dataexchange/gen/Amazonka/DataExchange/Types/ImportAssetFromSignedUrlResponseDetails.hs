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
-- Module      : Amazonka.DataExchange.Types.ImportAssetFromSignedUrlResponseDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ImportAssetFromSignedUrlResponseDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details in the response for an import request, including the signed
-- URL and other information.
--
-- /See:/ 'newImportAssetFromSignedUrlResponseDetails' smart constructor.
data ImportAssetFromSignedUrlResponseDetails = ImportAssetFromSignedUrlResponseDetails'
  { -- | The time and date at which the signed URL expires, in ISO 8601 format.
    signedUrlExpiresAt :: Prelude.Maybe Data.POSIX,
    -- | The signed URL.
    signedUrl :: Prelude.Maybe Prelude.Text,
    -- | The Base64-encoded Md5 hash for the asset, used to ensure the integrity
    -- of the file at that location.
    md5Hash :: Prelude.Maybe Prelude.Text,
    -- | The name for the asset associated with this import job.
    assetName :: Prelude.Text,
    -- | The unique identifier for the data set associated with this import job.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for the revision associated with this import
    -- response.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportAssetFromSignedUrlResponseDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signedUrlExpiresAt', 'importAssetFromSignedUrlResponseDetails_signedUrlExpiresAt' - The time and date at which the signed URL expires, in ISO 8601 format.
--
-- 'signedUrl', 'importAssetFromSignedUrlResponseDetails_signedUrl' - The signed URL.
--
-- 'md5Hash', 'importAssetFromSignedUrlResponseDetails_md5Hash' - The Base64-encoded Md5 hash for the asset, used to ensure the integrity
-- of the file at that location.
--
-- 'assetName', 'importAssetFromSignedUrlResponseDetails_assetName' - The name for the asset associated with this import job.
--
-- 'dataSetId', 'importAssetFromSignedUrlResponseDetails_dataSetId' - The unique identifier for the data set associated with this import job.
--
-- 'revisionId', 'importAssetFromSignedUrlResponseDetails_revisionId' - The unique identifier for the revision associated with this import
-- response.
newImportAssetFromSignedUrlResponseDetails ::
  -- | 'assetName'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  ImportAssetFromSignedUrlResponseDetails
newImportAssetFromSignedUrlResponseDetails
  pAssetName_
  pDataSetId_
  pRevisionId_ =
    ImportAssetFromSignedUrlResponseDetails'
      { signedUrlExpiresAt =
          Prelude.Nothing,
        signedUrl = Prelude.Nothing,
        md5Hash = Prelude.Nothing,
        assetName = pAssetName_,
        dataSetId = pDataSetId_,
        revisionId = pRevisionId_
      }

-- | The time and date at which the signed URL expires, in ISO 8601 format.
importAssetFromSignedUrlResponseDetails_signedUrlExpiresAt :: Lens.Lens' ImportAssetFromSignedUrlResponseDetails (Prelude.Maybe Prelude.UTCTime)
importAssetFromSignedUrlResponseDetails_signedUrlExpiresAt = Lens.lens (\ImportAssetFromSignedUrlResponseDetails' {signedUrlExpiresAt} -> signedUrlExpiresAt) (\s@ImportAssetFromSignedUrlResponseDetails' {} a -> s {signedUrlExpiresAt = a} :: ImportAssetFromSignedUrlResponseDetails) Prelude.. Lens.mapping Data._Time

-- | The signed URL.
importAssetFromSignedUrlResponseDetails_signedUrl :: Lens.Lens' ImportAssetFromSignedUrlResponseDetails (Prelude.Maybe Prelude.Text)
importAssetFromSignedUrlResponseDetails_signedUrl = Lens.lens (\ImportAssetFromSignedUrlResponseDetails' {signedUrl} -> signedUrl) (\s@ImportAssetFromSignedUrlResponseDetails' {} a -> s {signedUrl = a} :: ImportAssetFromSignedUrlResponseDetails)

-- | The Base64-encoded Md5 hash for the asset, used to ensure the integrity
-- of the file at that location.
importAssetFromSignedUrlResponseDetails_md5Hash :: Lens.Lens' ImportAssetFromSignedUrlResponseDetails (Prelude.Maybe Prelude.Text)
importAssetFromSignedUrlResponseDetails_md5Hash = Lens.lens (\ImportAssetFromSignedUrlResponseDetails' {md5Hash} -> md5Hash) (\s@ImportAssetFromSignedUrlResponseDetails' {} a -> s {md5Hash = a} :: ImportAssetFromSignedUrlResponseDetails)

-- | The name for the asset associated with this import job.
importAssetFromSignedUrlResponseDetails_assetName :: Lens.Lens' ImportAssetFromSignedUrlResponseDetails Prelude.Text
importAssetFromSignedUrlResponseDetails_assetName = Lens.lens (\ImportAssetFromSignedUrlResponseDetails' {assetName} -> assetName) (\s@ImportAssetFromSignedUrlResponseDetails' {} a -> s {assetName = a} :: ImportAssetFromSignedUrlResponseDetails)

-- | The unique identifier for the data set associated with this import job.
importAssetFromSignedUrlResponseDetails_dataSetId :: Lens.Lens' ImportAssetFromSignedUrlResponseDetails Prelude.Text
importAssetFromSignedUrlResponseDetails_dataSetId = Lens.lens (\ImportAssetFromSignedUrlResponseDetails' {dataSetId} -> dataSetId) (\s@ImportAssetFromSignedUrlResponseDetails' {} a -> s {dataSetId = a} :: ImportAssetFromSignedUrlResponseDetails)

-- | The unique identifier for the revision associated with this import
-- response.
importAssetFromSignedUrlResponseDetails_revisionId :: Lens.Lens' ImportAssetFromSignedUrlResponseDetails Prelude.Text
importAssetFromSignedUrlResponseDetails_revisionId = Lens.lens (\ImportAssetFromSignedUrlResponseDetails' {revisionId} -> revisionId) (\s@ImportAssetFromSignedUrlResponseDetails' {} a -> s {revisionId = a} :: ImportAssetFromSignedUrlResponseDetails)

instance
  Data.FromJSON
    ImportAssetFromSignedUrlResponseDetails
  where
  parseJSON =
    Data.withObject
      "ImportAssetFromSignedUrlResponseDetails"
      ( \x ->
          ImportAssetFromSignedUrlResponseDetails'
            Prelude.<$> (x Data..:? "SignedUrlExpiresAt")
            Prelude.<*> (x Data..:? "SignedUrl")
            Prelude.<*> (x Data..:? "Md5Hash")
            Prelude.<*> (x Data..: "AssetName")
            Prelude.<*> (x Data..: "DataSetId")
            Prelude.<*> (x Data..: "RevisionId")
      )

instance
  Prelude.Hashable
    ImportAssetFromSignedUrlResponseDetails
  where
  hashWithSalt
    _salt
    ImportAssetFromSignedUrlResponseDetails' {..} =
      _salt `Prelude.hashWithSalt` signedUrlExpiresAt
        `Prelude.hashWithSalt` signedUrl
        `Prelude.hashWithSalt` md5Hash
        `Prelude.hashWithSalt` assetName
        `Prelude.hashWithSalt` dataSetId
        `Prelude.hashWithSalt` revisionId

instance
  Prelude.NFData
    ImportAssetFromSignedUrlResponseDetails
  where
  rnf ImportAssetFromSignedUrlResponseDetails' {..} =
    Prelude.rnf signedUrlExpiresAt
      `Prelude.seq` Prelude.rnf signedUrl
      `Prelude.seq` Prelude.rnf md5Hash
      `Prelude.seq` Prelude.rnf assetName
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf revisionId
