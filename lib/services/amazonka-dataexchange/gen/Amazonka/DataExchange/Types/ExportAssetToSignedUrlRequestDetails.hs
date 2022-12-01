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
-- Module      : Amazonka.DataExchange.Types.ExportAssetToSignedUrlRequestDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ExportAssetToSignedUrlRequestDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details of the operation to be performed by the job.
--
-- /See:/ 'newExportAssetToSignedUrlRequestDetails' smart constructor.
data ExportAssetToSignedUrlRequestDetails = ExportAssetToSignedUrlRequestDetails'
  { -- | The unique identifier for the asset that is exported to a signed URL.
    assetId :: Prelude.Text,
    -- | The unique identifier for the data set associated with this export job.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for the revision associated with this export
    -- request.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportAssetToSignedUrlRequestDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetId', 'exportAssetToSignedUrlRequestDetails_assetId' - The unique identifier for the asset that is exported to a signed URL.
--
-- 'dataSetId', 'exportAssetToSignedUrlRequestDetails_dataSetId' - The unique identifier for the data set associated with this export job.
--
-- 'revisionId', 'exportAssetToSignedUrlRequestDetails_revisionId' - The unique identifier for the revision associated with this export
-- request.
newExportAssetToSignedUrlRequestDetails ::
  -- | 'assetId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  ExportAssetToSignedUrlRequestDetails
newExportAssetToSignedUrlRequestDetails
  pAssetId_
  pDataSetId_
  pRevisionId_ =
    ExportAssetToSignedUrlRequestDetails'
      { assetId =
          pAssetId_,
        dataSetId = pDataSetId_,
        revisionId = pRevisionId_
      }

-- | The unique identifier for the asset that is exported to a signed URL.
exportAssetToSignedUrlRequestDetails_assetId :: Lens.Lens' ExportAssetToSignedUrlRequestDetails Prelude.Text
exportAssetToSignedUrlRequestDetails_assetId = Lens.lens (\ExportAssetToSignedUrlRequestDetails' {assetId} -> assetId) (\s@ExportAssetToSignedUrlRequestDetails' {} a -> s {assetId = a} :: ExportAssetToSignedUrlRequestDetails)

-- | The unique identifier for the data set associated with this export job.
exportAssetToSignedUrlRequestDetails_dataSetId :: Lens.Lens' ExportAssetToSignedUrlRequestDetails Prelude.Text
exportAssetToSignedUrlRequestDetails_dataSetId = Lens.lens (\ExportAssetToSignedUrlRequestDetails' {dataSetId} -> dataSetId) (\s@ExportAssetToSignedUrlRequestDetails' {} a -> s {dataSetId = a} :: ExportAssetToSignedUrlRequestDetails)

-- | The unique identifier for the revision associated with this export
-- request.
exportAssetToSignedUrlRequestDetails_revisionId :: Lens.Lens' ExportAssetToSignedUrlRequestDetails Prelude.Text
exportAssetToSignedUrlRequestDetails_revisionId = Lens.lens (\ExportAssetToSignedUrlRequestDetails' {revisionId} -> revisionId) (\s@ExportAssetToSignedUrlRequestDetails' {} a -> s {revisionId = a} :: ExportAssetToSignedUrlRequestDetails)

instance
  Prelude.Hashable
    ExportAssetToSignedUrlRequestDetails
  where
  hashWithSalt
    _salt
    ExportAssetToSignedUrlRequestDetails' {..} =
      _salt `Prelude.hashWithSalt` assetId
        `Prelude.hashWithSalt` dataSetId
        `Prelude.hashWithSalt` revisionId

instance
  Prelude.NFData
    ExportAssetToSignedUrlRequestDetails
  where
  rnf ExportAssetToSignedUrlRequestDetails' {..} =
    Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf revisionId

instance
  Core.ToJSON
    ExportAssetToSignedUrlRequestDetails
  where
  toJSON ExportAssetToSignedUrlRequestDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AssetId" Core..= assetId),
            Prelude.Just ("DataSetId" Core..= dataSetId),
            Prelude.Just ("RevisionId" Core..= revisionId)
          ]
      )
