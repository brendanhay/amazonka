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
-- Module      : Amazonka.DataExchange.Types.ExportAssetsToS3ResponseDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ExportAssetsToS3ResponseDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types.AssetDestinationEntry
import Amazonka.DataExchange.Types.ExportServerSideEncryption
import qualified Amazonka.Prelude as Prelude

-- | Details about the export to Amazon S3 response.
--
-- /See:/ 'newExportAssetsToS3ResponseDetails' smart constructor.
data ExportAssetsToS3ResponseDetails = ExportAssetsToS3ResponseDetails'
  { -- | Encryption configuration of the export job.
    encryption :: Prelude.Maybe ExportServerSideEncryption,
    -- | The destination in Amazon S3 where the asset is exported.
    assetDestinations :: [AssetDestinationEntry],
    -- | The unique identifier for the data set associated with this export job.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for the revision associated with this export
    -- response.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportAssetsToS3ResponseDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryption', 'exportAssetsToS3ResponseDetails_encryption' - Encryption configuration of the export job.
--
-- 'assetDestinations', 'exportAssetsToS3ResponseDetails_assetDestinations' - The destination in Amazon S3 where the asset is exported.
--
-- 'dataSetId', 'exportAssetsToS3ResponseDetails_dataSetId' - The unique identifier for the data set associated with this export job.
--
-- 'revisionId', 'exportAssetsToS3ResponseDetails_revisionId' - The unique identifier for the revision associated with this export
-- response.
newExportAssetsToS3ResponseDetails ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  ExportAssetsToS3ResponseDetails
newExportAssetsToS3ResponseDetails
  pDataSetId_
  pRevisionId_ =
    ExportAssetsToS3ResponseDetails'
      { encryption =
          Prelude.Nothing,
        assetDestinations = Prelude.mempty,
        dataSetId = pDataSetId_,
        revisionId = pRevisionId_
      }

-- | Encryption configuration of the export job.
exportAssetsToS3ResponseDetails_encryption :: Lens.Lens' ExportAssetsToS3ResponseDetails (Prelude.Maybe ExportServerSideEncryption)
exportAssetsToS3ResponseDetails_encryption = Lens.lens (\ExportAssetsToS3ResponseDetails' {encryption} -> encryption) (\s@ExportAssetsToS3ResponseDetails' {} a -> s {encryption = a} :: ExportAssetsToS3ResponseDetails)

-- | The destination in Amazon S3 where the asset is exported.
exportAssetsToS3ResponseDetails_assetDestinations :: Lens.Lens' ExportAssetsToS3ResponseDetails [AssetDestinationEntry]
exportAssetsToS3ResponseDetails_assetDestinations = Lens.lens (\ExportAssetsToS3ResponseDetails' {assetDestinations} -> assetDestinations) (\s@ExportAssetsToS3ResponseDetails' {} a -> s {assetDestinations = a} :: ExportAssetsToS3ResponseDetails) Prelude.. Lens.coerced

-- | The unique identifier for the data set associated with this export job.
exportAssetsToS3ResponseDetails_dataSetId :: Lens.Lens' ExportAssetsToS3ResponseDetails Prelude.Text
exportAssetsToS3ResponseDetails_dataSetId = Lens.lens (\ExportAssetsToS3ResponseDetails' {dataSetId} -> dataSetId) (\s@ExportAssetsToS3ResponseDetails' {} a -> s {dataSetId = a} :: ExportAssetsToS3ResponseDetails)

-- | The unique identifier for the revision associated with this export
-- response.
exportAssetsToS3ResponseDetails_revisionId :: Lens.Lens' ExportAssetsToS3ResponseDetails Prelude.Text
exportAssetsToS3ResponseDetails_revisionId = Lens.lens (\ExportAssetsToS3ResponseDetails' {revisionId} -> revisionId) (\s@ExportAssetsToS3ResponseDetails' {} a -> s {revisionId = a} :: ExportAssetsToS3ResponseDetails)

instance
  Data.FromJSON
    ExportAssetsToS3ResponseDetails
  where
  parseJSON =
    Data.withObject
      "ExportAssetsToS3ResponseDetails"
      ( \x ->
          ExportAssetsToS3ResponseDetails'
            Prelude.<$> (x Data..:? "Encryption")
            Prelude.<*> ( x
                            Data..:? "AssetDestinations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "DataSetId")
            Prelude.<*> (x Data..: "RevisionId")
      )

instance
  Prelude.Hashable
    ExportAssetsToS3ResponseDetails
  where
  hashWithSalt
    _salt
    ExportAssetsToS3ResponseDetails' {..} =
      _salt
        `Prelude.hashWithSalt` encryption
        `Prelude.hashWithSalt` assetDestinations
        `Prelude.hashWithSalt` dataSetId
        `Prelude.hashWithSalt` revisionId

instance
  Prelude.NFData
    ExportAssetsToS3ResponseDetails
  where
  rnf ExportAssetsToS3ResponseDetails' {..} =
    Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf assetDestinations
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf revisionId
