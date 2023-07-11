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
-- Module      : Amazonka.IoTSiteWise.DeleteAsset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an asset. This action can\'t be undone. For more information,
-- see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/delete-assets-and-models.html Deleting assets and models>
-- in the /IoT SiteWise User Guide/.
--
-- You can\'t delete an asset that\'s associated to another asset. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_DisassociateAssets.html DisassociateAssets>.
module Amazonka.IoTSiteWise.DeleteAsset
  ( -- * Creating a Request
    DeleteAsset (..),
    newDeleteAsset,

    -- * Request Lenses
    deleteAsset_clientToken,
    deleteAsset_assetId,

    -- * Destructuring the Response
    DeleteAssetResponse (..),
    newDeleteAssetResponse,

    -- * Response Lenses
    deleteAssetResponse_httpStatus,
    deleteAssetResponse_assetStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAsset' smart constructor.
data DeleteAsset = DeleteAsset'
  { -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset to delete.
    assetId :: Prelude.Text
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
-- 'clientToken', 'deleteAsset_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'assetId', 'deleteAsset_assetId' - The ID of the asset to delete.
newDeleteAsset ::
  -- | 'assetId'
  Prelude.Text ->
  DeleteAsset
newDeleteAsset pAssetId_ =
  DeleteAsset'
    { clientToken = Prelude.Nothing,
      assetId = pAssetId_
    }

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
deleteAsset_clientToken :: Lens.Lens' DeleteAsset (Prelude.Maybe Prelude.Text)
deleteAsset_clientToken = Lens.lens (\DeleteAsset' {clientToken} -> clientToken) (\s@DeleteAsset' {} a -> s {clientToken = a} :: DeleteAsset)

-- | The ID of the asset to delete.
deleteAsset_assetId :: Lens.Lens' DeleteAsset Prelude.Text
deleteAsset_assetId = Lens.lens (\DeleteAsset' {assetId} -> assetId) (\s@DeleteAsset' {} a -> s {assetId = a} :: DeleteAsset)

instance Core.AWSRequest DeleteAsset where
  type AWSResponse DeleteAsset = DeleteAssetResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAssetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "assetStatus")
      )

instance Prelude.Hashable DeleteAsset where
  hashWithSalt _salt DeleteAsset' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` assetId

instance Prelude.NFData DeleteAsset where
  rnf DeleteAsset' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf assetId

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
    Prelude.mconcat ["/assets/", Data.toBS assetId]

instance Data.ToQuery DeleteAsset where
  toQuery DeleteAsset' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newDeleteAssetResponse' smart constructor.
data DeleteAssetResponse = DeleteAssetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the asset, which contains a state (@DELETING@ after
    -- successfully calling this operation) and any error message.
    assetStatus :: AssetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAssetResponse_httpStatus' - The response's http status code.
--
-- 'assetStatus', 'deleteAssetResponse_assetStatus' - The status of the asset, which contains a state (@DELETING@ after
-- successfully calling this operation) and any error message.
newDeleteAssetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'assetStatus'
  AssetStatus ->
  DeleteAssetResponse
newDeleteAssetResponse pHttpStatus_ pAssetStatus_ =
  DeleteAssetResponse'
    { httpStatus = pHttpStatus_,
      assetStatus = pAssetStatus_
    }

-- | The response's http status code.
deleteAssetResponse_httpStatus :: Lens.Lens' DeleteAssetResponse Prelude.Int
deleteAssetResponse_httpStatus = Lens.lens (\DeleteAssetResponse' {httpStatus} -> httpStatus) (\s@DeleteAssetResponse' {} a -> s {httpStatus = a} :: DeleteAssetResponse)

-- | The status of the asset, which contains a state (@DELETING@ after
-- successfully calling this operation) and any error message.
deleteAssetResponse_assetStatus :: Lens.Lens' DeleteAssetResponse AssetStatus
deleteAssetResponse_assetStatus = Lens.lens (\DeleteAssetResponse' {assetStatus} -> assetStatus) (\s@DeleteAssetResponse' {} a -> s {assetStatus = a} :: DeleteAssetResponse)

instance Prelude.NFData DeleteAssetResponse where
  rnf DeleteAssetResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assetStatus
