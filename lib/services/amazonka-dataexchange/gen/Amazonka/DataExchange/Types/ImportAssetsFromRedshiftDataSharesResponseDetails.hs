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
-- Module      : Amazonka.DataExchange.Types.ImportAssetsFromRedshiftDataSharesResponseDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ImportAssetsFromRedshiftDataSharesResponseDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataExchange.Types.RedshiftDataShareAssetSourceEntry
import qualified Amazonka.Prelude as Prelude

-- | Details from an import from Amazon Redshift datashare response.
--
-- /See:/ 'newImportAssetsFromRedshiftDataSharesResponseDetails' smart constructor.
data ImportAssetsFromRedshiftDataSharesResponseDetails = ImportAssetsFromRedshiftDataSharesResponseDetails'
  { -- | A list of Amazon Redshift datashare asset sources.
    assetSources :: [RedshiftDataShareAssetSourceEntry],
    -- | The unique identifier for the data set associated with this import job.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for the revision associated with this import job.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportAssetsFromRedshiftDataSharesResponseDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetSources', 'importAssetsFromRedshiftDataSharesResponseDetails_assetSources' - A list of Amazon Redshift datashare asset sources.
--
-- 'dataSetId', 'importAssetsFromRedshiftDataSharesResponseDetails_dataSetId' - The unique identifier for the data set associated with this import job.
--
-- 'revisionId', 'importAssetsFromRedshiftDataSharesResponseDetails_revisionId' - The unique identifier for the revision associated with this import job.
newImportAssetsFromRedshiftDataSharesResponseDetails ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  ImportAssetsFromRedshiftDataSharesResponseDetails
newImportAssetsFromRedshiftDataSharesResponseDetails
  pDataSetId_
  pRevisionId_ =
    ImportAssetsFromRedshiftDataSharesResponseDetails'
      { assetSources =
          Prelude.mempty,
        dataSetId = pDataSetId_,
        revisionId =
          pRevisionId_
      }

-- | A list of Amazon Redshift datashare asset sources.
importAssetsFromRedshiftDataSharesResponseDetails_assetSources :: Lens.Lens' ImportAssetsFromRedshiftDataSharesResponseDetails [RedshiftDataShareAssetSourceEntry]
importAssetsFromRedshiftDataSharesResponseDetails_assetSources = Lens.lens (\ImportAssetsFromRedshiftDataSharesResponseDetails' {assetSources} -> assetSources) (\s@ImportAssetsFromRedshiftDataSharesResponseDetails' {} a -> s {assetSources = a} :: ImportAssetsFromRedshiftDataSharesResponseDetails) Prelude.. Lens.coerced

-- | The unique identifier for the data set associated with this import job.
importAssetsFromRedshiftDataSharesResponseDetails_dataSetId :: Lens.Lens' ImportAssetsFromRedshiftDataSharesResponseDetails Prelude.Text
importAssetsFromRedshiftDataSharesResponseDetails_dataSetId = Lens.lens (\ImportAssetsFromRedshiftDataSharesResponseDetails' {dataSetId} -> dataSetId) (\s@ImportAssetsFromRedshiftDataSharesResponseDetails' {} a -> s {dataSetId = a} :: ImportAssetsFromRedshiftDataSharesResponseDetails)

-- | The unique identifier for the revision associated with this import job.
importAssetsFromRedshiftDataSharesResponseDetails_revisionId :: Lens.Lens' ImportAssetsFromRedshiftDataSharesResponseDetails Prelude.Text
importAssetsFromRedshiftDataSharesResponseDetails_revisionId = Lens.lens (\ImportAssetsFromRedshiftDataSharesResponseDetails' {revisionId} -> revisionId) (\s@ImportAssetsFromRedshiftDataSharesResponseDetails' {} a -> s {revisionId = a} :: ImportAssetsFromRedshiftDataSharesResponseDetails)

instance
  Core.FromJSON
    ImportAssetsFromRedshiftDataSharesResponseDetails
  where
  parseJSON =
    Core.withObject
      "ImportAssetsFromRedshiftDataSharesResponseDetails"
      ( \x ->
          ImportAssetsFromRedshiftDataSharesResponseDetails'
            Prelude.<$> (x Core..:? "AssetSources" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..: "DataSetId")
              Prelude.<*> (x Core..: "RevisionId")
      )

instance
  Prelude.Hashable
    ImportAssetsFromRedshiftDataSharesResponseDetails
  where
  hashWithSalt
    _salt
    ImportAssetsFromRedshiftDataSharesResponseDetails' {..} =
      _salt `Prelude.hashWithSalt` assetSources
        `Prelude.hashWithSalt` dataSetId
        `Prelude.hashWithSalt` revisionId

instance
  Prelude.NFData
    ImportAssetsFromRedshiftDataSharesResponseDetails
  where
  rnf
    ImportAssetsFromRedshiftDataSharesResponseDetails' {..} =
      Prelude.rnf assetSources
        `Prelude.seq` Prelude.rnf dataSetId
        `Prelude.seq` Prelude.rnf revisionId
