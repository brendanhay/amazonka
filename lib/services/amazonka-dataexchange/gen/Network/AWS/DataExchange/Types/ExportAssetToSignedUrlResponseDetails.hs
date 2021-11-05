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
-- Module      : Network.AWS.DataExchange.Types.ExportAssetToSignedUrlResponseDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataExchange.Types.ExportAssetToSignedUrlResponseDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details of the export to signed URL response.
--
-- /See:/ 'newExportAssetToSignedUrlResponseDetails' smart constructor.
data ExportAssetToSignedUrlResponseDetails = ExportAssetToSignedUrlResponseDetails'
  { -- | The signed URL for the export request.
    signedUrl :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the signed URL expires, in ISO 8601 format.
    signedUrlExpiresAt :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier for the data set associated with this export job.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for the asset associated with this export job.
    assetId :: Prelude.Text,
    -- | The unique identifier for the revision associated with this export
    -- response.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportAssetToSignedUrlResponseDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signedUrl', 'exportAssetToSignedUrlResponseDetails_signedUrl' - The signed URL for the export request.
--
-- 'signedUrlExpiresAt', 'exportAssetToSignedUrlResponseDetails_signedUrlExpiresAt' - The date and time that the signed URL expires, in ISO 8601 format.
--
-- 'dataSetId', 'exportAssetToSignedUrlResponseDetails_dataSetId' - The unique identifier for the data set associated with this export job.
--
-- 'assetId', 'exportAssetToSignedUrlResponseDetails_assetId' - The unique identifier for the asset associated with this export job.
--
-- 'revisionId', 'exportAssetToSignedUrlResponseDetails_revisionId' - The unique identifier for the revision associated with this export
-- response.
newExportAssetToSignedUrlResponseDetails ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'assetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  ExportAssetToSignedUrlResponseDetails
newExportAssetToSignedUrlResponseDetails
  pDataSetId_
  pAssetId_
  pRevisionId_ =
    ExportAssetToSignedUrlResponseDetails'
      { signedUrl =
          Prelude.Nothing,
        signedUrlExpiresAt = Prelude.Nothing,
        dataSetId = pDataSetId_,
        assetId = pAssetId_,
        revisionId = pRevisionId_
      }

-- | The signed URL for the export request.
exportAssetToSignedUrlResponseDetails_signedUrl :: Lens.Lens' ExportAssetToSignedUrlResponseDetails (Prelude.Maybe Prelude.Text)
exportAssetToSignedUrlResponseDetails_signedUrl = Lens.lens (\ExportAssetToSignedUrlResponseDetails' {signedUrl} -> signedUrl) (\s@ExportAssetToSignedUrlResponseDetails' {} a -> s {signedUrl = a} :: ExportAssetToSignedUrlResponseDetails)

-- | The date and time that the signed URL expires, in ISO 8601 format.
exportAssetToSignedUrlResponseDetails_signedUrlExpiresAt :: Lens.Lens' ExportAssetToSignedUrlResponseDetails (Prelude.Maybe Prelude.UTCTime)
exportAssetToSignedUrlResponseDetails_signedUrlExpiresAt = Lens.lens (\ExportAssetToSignedUrlResponseDetails' {signedUrlExpiresAt} -> signedUrlExpiresAt) (\s@ExportAssetToSignedUrlResponseDetails' {} a -> s {signedUrlExpiresAt = a} :: ExportAssetToSignedUrlResponseDetails) Prelude.. Lens.mapping Core._Time

-- | The unique identifier for the data set associated with this export job.
exportAssetToSignedUrlResponseDetails_dataSetId :: Lens.Lens' ExportAssetToSignedUrlResponseDetails Prelude.Text
exportAssetToSignedUrlResponseDetails_dataSetId = Lens.lens (\ExportAssetToSignedUrlResponseDetails' {dataSetId} -> dataSetId) (\s@ExportAssetToSignedUrlResponseDetails' {} a -> s {dataSetId = a} :: ExportAssetToSignedUrlResponseDetails)

-- | The unique identifier for the asset associated with this export job.
exportAssetToSignedUrlResponseDetails_assetId :: Lens.Lens' ExportAssetToSignedUrlResponseDetails Prelude.Text
exportAssetToSignedUrlResponseDetails_assetId = Lens.lens (\ExportAssetToSignedUrlResponseDetails' {assetId} -> assetId) (\s@ExportAssetToSignedUrlResponseDetails' {} a -> s {assetId = a} :: ExportAssetToSignedUrlResponseDetails)

-- | The unique identifier for the revision associated with this export
-- response.
exportAssetToSignedUrlResponseDetails_revisionId :: Lens.Lens' ExportAssetToSignedUrlResponseDetails Prelude.Text
exportAssetToSignedUrlResponseDetails_revisionId = Lens.lens (\ExportAssetToSignedUrlResponseDetails' {revisionId} -> revisionId) (\s@ExportAssetToSignedUrlResponseDetails' {} a -> s {revisionId = a} :: ExportAssetToSignedUrlResponseDetails)

instance
  Core.FromJSON
    ExportAssetToSignedUrlResponseDetails
  where
  parseJSON =
    Core.withObject
      "ExportAssetToSignedUrlResponseDetails"
      ( \x ->
          ExportAssetToSignedUrlResponseDetails'
            Prelude.<$> (x Core..:? "SignedUrl")
            Prelude.<*> (x Core..:? "SignedUrlExpiresAt")
            Prelude.<*> (x Core..: "DataSetId")
            Prelude.<*> (x Core..: "AssetId")
            Prelude.<*> (x Core..: "RevisionId")
      )

instance
  Prelude.Hashable
    ExportAssetToSignedUrlResponseDetails

instance
  Prelude.NFData
    ExportAssetToSignedUrlResponseDetails
