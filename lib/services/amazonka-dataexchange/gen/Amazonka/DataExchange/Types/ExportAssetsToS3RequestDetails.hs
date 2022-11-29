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
-- Module      : Amazonka.DataExchange.Types.ExportAssetsToS3RequestDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ExportAssetsToS3RequestDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataExchange.Types.AssetDestinationEntry
import Amazonka.DataExchange.Types.ExportServerSideEncryption
import qualified Amazonka.Prelude as Prelude

-- | Details of the operation to be performed by the job.
--
-- /See:/ 'newExportAssetsToS3RequestDetails' smart constructor.
data ExportAssetsToS3RequestDetails = ExportAssetsToS3RequestDetails'
  { -- | Encryption configuration for the export job.
    encryption :: Prelude.Maybe ExportServerSideEncryption,
    -- | The destination for the asset.
    assetDestinations :: [AssetDestinationEntry],
    -- | The unique identifier for the data set associated with this export job.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for the revision associated with this export
    -- request.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportAssetsToS3RequestDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryption', 'exportAssetsToS3RequestDetails_encryption' - Encryption configuration for the export job.
--
-- 'assetDestinations', 'exportAssetsToS3RequestDetails_assetDestinations' - The destination for the asset.
--
-- 'dataSetId', 'exportAssetsToS3RequestDetails_dataSetId' - The unique identifier for the data set associated with this export job.
--
-- 'revisionId', 'exportAssetsToS3RequestDetails_revisionId' - The unique identifier for the revision associated with this export
-- request.
newExportAssetsToS3RequestDetails ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  ExportAssetsToS3RequestDetails
newExportAssetsToS3RequestDetails
  pDataSetId_
  pRevisionId_ =
    ExportAssetsToS3RequestDetails'
      { encryption =
          Prelude.Nothing,
        assetDestinations = Prelude.mempty,
        dataSetId = pDataSetId_,
        revisionId = pRevisionId_
      }

-- | Encryption configuration for the export job.
exportAssetsToS3RequestDetails_encryption :: Lens.Lens' ExportAssetsToS3RequestDetails (Prelude.Maybe ExportServerSideEncryption)
exportAssetsToS3RequestDetails_encryption = Lens.lens (\ExportAssetsToS3RequestDetails' {encryption} -> encryption) (\s@ExportAssetsToS3RequestDetails' {} a -> s {encryption = a} :: ExportAssetsToS3RequestDetails)

-- | The destination for the asset.
exportAssetsToS3RequestDetails_assetDestinations :: Lens.Lens' ExportAssetsToS3RequestDetails [AssetDestinationEntry]
exportAssetsToS3RequestDetails_assetDestinations = Lens.lens (\ExportAssetsToS3RequestDetails' {assetDestinations} -> assetDestinations) (\s@ExportAssetsToS3RequestDetails' {} a -> s {assetDestinations = a} :: ExportAssetsToS3RequestDetails) Prelude.. Lens.coerced

-- | The unique identifier for the data set associated with this export job.
exportAssetsToS3RequestDetails_dataSetId :: Lens.Lens' ExportAssetsToS3RequestDetails Prelude.Text
exportAssetsToS3RequestDetails_dataSetId = Lens.lens (\ExportAssetsToS3RequestDetails' {dataSetId} -> dataSetId) (\s@ExportAssetsToS3RequestDetails' {} a -> s {dataSetId = a} :: ExportAssetsToS3RequestDetails)

-- | The unique identifier for the revision associated with this export
-- request.
exportAssetsToS3RequestDetails_revisionId :: Lens.Lens' ExportAssetsToS3RequestDetails Prelude.Text
exportAssetsToS3RequestDetails_revisionId = Lens.lens (\ExportAssetsToS3RequestDetails' {revisionId} -> revisionId) (\s@ExportAssetsToS3RequestDetails' {} a -> s {revisionId = a} :: ExportAssetsToS3RequestDetails)

instance
  Prelude.Hashable
    ExportAssetsToS3RequestDetails
  where
  hashWithSalt
    _salt
    ExportAssetsToS3RequestDetails' {..} =
      _salt `Prelude.hashWithSalt` encryption
        `Prelude.hashWithSalt` assetDestinations
        `Prelude.hashWithSalt` dataSetId
        `Prelude.hashWithSalt` revisionId

instance
  Prelude.NFData
    ExportAssetsToS3RequestDetails
  where
  rnf ExportAssetsToS3RequestDetails' {..} =
    Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf assetDestinations
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf revisionId

instance Core.ToJSON ExportAssetsToS3RequestDetails where
  toJSON ExportAssetsToS3RequestDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Encryption" Core..=) Prelude.<$> encryption,
            Prelude.Just
              ("AssetDestinations" Core..= assetDestinations),
            Prelude.Just ("DataSetId" Core..= dataSetId),
            Prelude.Just ("RevisionId" Core..= revisionId)
          ]
      )
