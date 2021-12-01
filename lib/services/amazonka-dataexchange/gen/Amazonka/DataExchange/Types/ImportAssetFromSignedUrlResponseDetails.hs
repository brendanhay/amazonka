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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ImportAssetFromSignedUrlResponseDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details in the response for an import request, including the signed
-- URL and other information.
--
-- /See:/ 'newImportAssetFromSignedUrlResponseDetails' smart constructor.
data ImportAssetFromSignedUrlResponseDetails = ImportAssetFromSignedUrlResponseDetails'
  { -- | The signed URL.
    signedUrl :: Prelude.Maybe Prelude.Text,
    -- | The time and date at which the signed URL expires, in ISO 8601 format.
    signedUrlExpiresAt :: Prelude.Maybe Core.POSIX,
    -- | The Base64-encoded Md5 hash for the asset, used to ensure the integrity
    -- of the file at that location.
    md5Hash :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the data set associated with this import job.
    dataSetId :: Prelude.Text,
    -- | The name for the asset associated with this import job.
    assetName :: Prelude.Text,
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
-- 'signedUrl', 'importAssetFromSignedUrlResponseDetails_signedUrl' - The signed URL.
--
-- 'signedUrlExpiresAt', 'importAssetFromSignedUrlResponseDetails_signedUrlExpiresAt' - The time and date at which the signed URL expires, in ISO 8601 format.
--
-- 'md5Hash', 'importAssetFromSignedUrlResponseDetails_md5Hash' - The Base64-encoded Md5 hash for the asset, used to ensure the integrity
-- of the file at that location.
--
-- 'dataSetId', 'importAssetFromSignedUrlResponseDetails_dataSetId' - The unique identifier for the data set associated with this import job.
--
-- 'assetName', 'importAssetFromSignedUrlResponseDetails_assetName' - The name for the asset associated with this import job.
--
-- 'revisionId', 'importAssetFromSignedUrlResponseDetails_revisionId' - The unique identifier for the revision associated with this import
-- response.
newImportAssetFromSignedUrlResponseDetails ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'assetName'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  ImportAssetFromSignedUrlResponseDetails
newImportAssetFromSignedUrlResponseDetails
  pDataSetId_
  pAssetName_
  pRevisionId_ =
    ImportAssetFromSignedUrlResponseDetails'
      { signedUrl =
          Prelude.Nothing,
        signedUrlExpiresAt =
          Prelude.Nothing,
        md5Hash = Prelude.Nothing,
        dataSetId = pDataSetId_,
        assetName = pAssetName_,
        revisionId = pRevisionId_
      }

-- | The signed URL.
importAssetFromSignedUrlResponseDetails_signedUrl :: Lens.Lens' ImportAssetFromSignedUrlResponseDetails (Prelude.Maybe Prelude.Text)
importAssetFromSignedUrlResponseDetails_signedUrl = Lens.lens (\ImportAssetFromSignedUrlResponseDetails' {signedUrl} -> signedUrl) (\s@ImportAssetFromSignedUrlResponseDetails' {} a -> s {signedUrl = a} :: ImportAssetFromSignedUrlResponseDetails)

-- | The time and date at which the signed URL expires, in ISO 8601 format.
importAssetFromSignedUrlResponseDetails_signedUrlExpiresAt :: Lens.Lens' ImportAssetFromSignedUrlResponseDetails (Prelude.Maybe Prelude.UTCTime)
importAssetFromSignedUrlResponseDetails_signedUrlExpiresAt = Lens.lens (\ImportAssetFromSignedUrlResponseDetails' {signedUrlExpiresAt} -> signedUrlExpiresAt) (\s@ImportAssetFromSignedUrlResponseDetails' {} a -> s {signedUrlExpiresAt = a} :: ImportAssetFromSignedUrlResponseDetails) Prelude.. Lens.mapping Core._Time

-- | The Base64-encoded Md5 hash for the asset, used to ensure the integrity
-- of the file at that location.
importAssetFromSignedUrlResponseDetails_md5Hash :: Lens.Lens' ImportAssetFromSignedUrlResponseDetails (Prelude.Maybe Prelude.Text)
importAssetFromSignedUrlResponseDetails_md5Hash = Lens.lens (\ImportAssetFromSignedUrlResponseDetails' {md5Hash} -> md5Hash) (\s@ImportAssetFromSignedUrlResponseDetails' {} a -> s {md5Hash = a} :: ImportAssetFromSignedUrlResponseDetails)

-- | The unique identifier for the data set associated with this import job.
importAssetFromSignedUrlResponseDetails_dataSetId :: Lens.Lens' ImportAssetFromSignedUrlResponseDetails Prelude.Text
importAssetFromSignedUrlResponseDetails_dataSetId = Lens.lens (\ImportAssetFromSignedUrlResponseDetails' {dataSetId} -> dataSetId) (\s@ImportAssetFromSignedUrlResponseDetails' {} a -> s {dataSetId = a} :: ImportAssetFromSignedUrlResponseDetails)

-- | The name for the asset associated with this import job.
importAssetFromSignedUrlResponseDetails_assetName :: Lens.Lens' ImportAssetFromSignedUrlResponseDetails Prelude.Text
importAssetFromSignedUrlResponseDetails_assetName = Lens.lens (\ImportAssetFromSignedUrlResponseDetails' {assetName} -> assetName) (\s@ImportAssetFromSignedUrlResponseDetails' {} a -> s {assetName = a} :: ImportAssetFromSignedUrlResponseDetails)

-- | The unique identifier for the revision associated with this import
-- response.
importAssetFromSignedUrlResponseDetails_revisionId :: Lens.Lens' ImportAssetFromSignedUrlResponseDetails Prelude.Text
importAssetFromSignedUrlResponseDetails_revisionId = Lens.lens (\ImportAssetFromSignedUrlResponseDetails' {revisionId} -> revisionId) (\s@ImportAssetFromSignedUrlResponseDetails' {} a -> s {revisionId = a} :: ImportAssetFromSignedUrlResponseDetails)

instance
  Core.FromJSON
    ImportAssetFromSignedUrlResponseDetails
  where
  parseJSON =
    Core.withObject
      "ImportAssetFromSignedUrlResponseDetails"
      ( \x ->
          ImportAssetFromSignedUrlResponseDetails'
            Prelude.<$> (x Core..:? "SignedUrl")
            Prelude.<*> (x Core..:? "SignedUrlExpiresAt")
            Prelude.<*> (x Core..:? "Md5Hash")
            Prelude.<*> (x Core..: "DataSetId")
            Prelude.<*> (x Core..: "AssetName")
            Prelude.<*> (x Core..: "RevisionId")
      )

instance
  Prelude.Hashable
    ImportAssetFromSignedUrlResponseDetails
  where
  hashWithSalt
    salt'
    ImportAssetFromSignedUrlResponseDetails' {..} =
      salt' `Prelude.hashWithSalt` revisionId
        `Prelude.hashWithSalt` assetName
        `Prelude.hashWithSalt` dataSetId
        `Prelude.hashWithSalt` md5Hash
        `Prelude.hashWithSalt` signedUrlExpiresAt
        `Prelude.hashWithSalt` signedUrl

instance
  Prelude.NFData
    ImportAssetFromSignedUrlResponseDetails
  where
  rnf ImportAssetFromSignedUrlResponseDetails' {..} =
    Prelude.rnf signedUrl
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf assetName
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf md5Hash
      `Prelude.seq` Prelude.rnf signedUrlExpiresAt
