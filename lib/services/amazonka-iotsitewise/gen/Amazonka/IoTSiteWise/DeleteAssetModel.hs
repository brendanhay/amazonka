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
-- Module      : Amazonka.IoTSiteWise.DeleteAssetModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an asset model. This action can\'t be undone. You must delete
-- all assets created from an asset model before you can delete the model.
-- Also, you can\'t delete an asset model if a parent asset model exists
-- that contains a property formula expression that depends on the asset
-- model that you want to delete. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/delete-assets-and-models.html Deleting assets and models>
-- in the /IoT SiteWise User Guide/.
module Amazonka.IoTSiteWise.DeleteAssetModel
  ( -- * Creating a Request
    DeleteAssetModel (..),
    newDeleteAssetModel,

    -- * Request Lenses
    deleteAssetModel_clientToken,
    deleteAssetModel_assetModelId,

    -- * Destructuring the Response
    DeleteAssetModelResponse (..),
    newDeleteAssetModelResponse,

    -- * Response Lenses
    deleteAssetModelResponse_httpStatus,
    deleteAssetModelResponse_assetModelStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAssetModel' smart constructor.
data DeleteAssetModel = DeleteAssetModel'
  { -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset model to delete.
    assetModelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssetModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteAssetModel_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'assetModelId', 'deleteAssetModel_assetModelId' - The ID of the asset model to delete.
newDeleteAssetModel ::
  -- | 'assetModelId'
  Prelude.Text ->
  DeleteAssetModel
newDeleteAssetModel pAssetModelId_ =
  DeleteAssetModel'
    { clientToken = Prelude.Nothing,
      assetModelId = pAssetModelId_
    }

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
deleteAssetModel_clientToken :: Lens.Lens' DeleteAssetModel (Prelude.Maybe Prelude.Text)
deleteAssetModel_clientToken = Lens.lens (\DeleteAssetModel' {clientToken} -> clientToken) (\s@DeleteAssetModel' {} a -> s {clientToken = a} :: DeleteAssetModel)

-- | The ID of the asset model to delete.
deleteAssetModel_assetModelId :: Lens.Lens' DeleteAssetModel Prelude.Text
deleteAssetModel_assetModelId = Lens.lens (\DeleteAssetModel' {assetModelId} -> assetModelId) (\s@DeleteAssetModel' {} a -> s {assetModelId = a} :: DeleteAssetModel)

instance Core.AWSRequest DeleteAssetModel where
  type
    AWSResponse DeleteAssetModel =
      DeleteAssetModelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAssetModelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "assetModelStatus")
      )

instance Prelude.Hashable DeleteAssetModel where
  hashWithSalt _salt DeleteAssetModel' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` assetModelId

instance Prelude.NFData DeleteAssetModel where
  rnf DeleteAssetModel' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf assetModelId

instance Data.ToHeaders DeleteAssetModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteAssetModel where
  toPath DeleteAssetModel' {..} =
    Prelude.mconcat
      ["/asset-models/", Data.toBS assetModelId]

instance Data.ToQuery DeleteAssetModel where
  toQuery DeleteAssetModel' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newDeleteAssetModelResponse' smart constructor.
data DeleteAssetModelResponse = DeleteAssetModelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the asset model, which contains a state (@DELETING@ after
    -- successfully calling this operation) and any error message.
    assetModelStatus :: AssetModelStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssetModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAssetModelResponse_httpStatus' - The response's http status code.
--
-- 'assetModelStatus', 'deleteAssetModelResponse_assetModelStatus' - The status of the asset model, which contains a state (@DELETING@ after
-- successfully calling this operation) and any error message.
newDeleteAssetModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'assetModelStatus'
  AssetModelStatus ->
  DeleteAssetModelResponse
newDeleteAssetModelResponse
  pHttpStatus_
  pAssetModelStatus_ =
    DeleteAssetModelResponse'
      { httpStatus =
          pHttpStatus_,
        assetModelStatus = pAssetModelStatus_
      }

-- | The response's http status code.
deleteAssetModelResponse_httpStatus :: Lens.Lens' DeleteAssetModelResponse Prelude.Int
deleteAssetModelResponse_httpStatus = Lens.lens (\DeleteAssetModelResponse' {httpStatus} -> httpStatus) (\s@DeleteAssetModelResponse' {} a -> s {httpStatus = a} :: DeleteAssetModelResponse)

-- | The status of the asset model, which contains a state (@DELETING@ after
-- successfully calling this operation) and any error message.
deleteAssetModelResponse_assetModelStatus :: Lens.Lens' DeleteAssetModelResponse AssetModelStatus
deleteAssetModelResponse_assetModelStatus = Lens.lens (\DeleteAssetModelResponse' {assetModelStatus} -> assetModelStatus) (\s@DeleteAssetModelResponse' {} a -> s {assetModelStatus = a} :: DeleteAssetModelResponse)

instance Prelude.NFData DeleteAssetModelResponse where
  rnf DeleteAssetModelResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assetModelStatus
