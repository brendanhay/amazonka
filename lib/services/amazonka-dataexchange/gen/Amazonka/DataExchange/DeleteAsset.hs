{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataExchange.DeleteAsset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes an asset.
module Amazonka.DataExchange.DeleteAsset
  ( -- * Creating a Request
    DeleteAsset (..),
    newDeleteAsset,

    -- * Request Lenses
    deleteAsset_assetId,
    deleteAsset_dataSetId,
    deleteAsset_revisionId,

    -- * Destructuring the Response
    DeleteAssetResponse (..),
    newDeleteAssetResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAsset' smart constructor.
data DeleteAsset = DeleteAsset'
  { -- | The unique identifier for an asset.
    assetId :: Prelude.Text,
    -- | The unique identifier for a data set.
    dataSetId :: Prelude.Text,
    -- | The unique identifier for a revision.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAsset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetId', 'deleteAsset_assetId' - The unique identifier for an asset.
--
-- 'dataSetId', 'deleteAsset_dataSetId' - The unique identifier for a data set.
--
-- 'revisionId', 'deleteAsset_revisionId' - The unique identifier for a revision.
newDeleteAsset ::
  -- | 'assetId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  DeleteAsset
newDeleteAsset pAssetId_ pDataSetId_ pRevisionId_ =
  DeleteAsset'
    { assetId = pAssetId_,
      dataSetId = pDataSetId_,
      revisionId = pRevisionId_
    }

-- | The unique identifier for an asset.
deleteAsset_assetId :: Lens.Lens' DeleteAsset Prelude.Text
deleteAsset_assetId = Lens.lens (\DeleteAsset' {assetId} -> assetId) (\s@DeleteAsset' {} a -> s {assetId = a} :: DeleteAsset)

-- | The unique identifier for a data set.
deleteAsset_dataSetId :: Lens.Lens' DeleteAsset Prelude.Text
deleteAsset_dataSetId = Lens.lens (\DeleteAsset' {dataSetId} -> dataSetId) (\s@DeleteAsset' {} a -> s {dataSetId = a} :: DeleteAsset)

-- | The unique identifier for a revision.
deleteAsset_revisionId :: Lens.Lens' DeleteAsset Prelude.Text
deleteAsset_revisionId = Lens.lens (\DeleteAsset' {revisionId} -> revisionId) (\s@DeleteAsset' {} a -> s {revisionId = a} :: DeleteAsset)

instance Core.AWSRequest DeleteAsset where
  type AWSResponse DeleteAsset = DeleteAssetResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteAssetResponse'

instance Prelude.Hashable DeleteAsset where
  hashWithSalt _salt DeleteAsset' {..} =
    _salt `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` revisionId

instance Prelude.NFData DeleteAsset where
  rnf DeleteAsset' {..} =
    Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf revisionId

instance Data.ToHeaders DeleteAsset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteAsset where
  toPath DeleteAsset' {..} =
    Prelude.mconcat
      [ "/v1/data-sets/",
        Data.toBS dataSetId,
        "/revisions/",
        Data.toBS revisionId,
        "/assets/",
        Data.toBS assetId
      ]

instance Data.ToQuery DeleteAsset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAssetResponse' smart constructor.
data DeleteAssetResponse = DeleteAssetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAssetResponse ::
  DeleteAssetResponse
newDeleteAssetResponse = DeleteAssetResponse'

instance Prelude.NFData DeleteAssetResponse where
  rnf _ = ()
