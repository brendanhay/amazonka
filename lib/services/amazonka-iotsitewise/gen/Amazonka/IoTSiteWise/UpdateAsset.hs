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
-- Module      : Amazonka.IoTSiteWise.UpdateAsset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an asset\'s name. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/update-assets-and-models.html Updating assets and models>
-- in the /IoT SiteWise User Guide/.
module Amazonka.IoTSiteWise.UpdateAsset
  ( -- * Creating a Request
    UpdateAsset (..),
    newUpdateAsset,

    -- * Request Lenses
    updateAsset_assetDescription,
    updateAsset_clientToken,
    updateAsset_assetId,
    updateAsset_assetName,

    -- * Destructuring the Response
    UpdateAssetResponse (..),
    newUpdateAssetResponse,

    -- * Response Lenses
    updateAssetResponse_httpStatus,
    updateAssetResponse_assetStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAsset' smart constructor.
data UpdateAsset = UpdateAsset'
  { -- | A description for the asset.
    assetDescription :: Prelude.Maybe Prelude.Text,
    -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset to update.
    assetId :: Prelude.Text,
    -- | A friendly name for the asset.
    assetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAsset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetDescription', 'updateAsset_assetDescription' - A description for the asset.
--
-- 'clientToken', 'updateAsset_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'assetId', 'updateAsset_assetId' - The ID of the asset to update.
--
-- 'assetName', 'updateAsset_assetName' - A friendly name for the asset.
newUpdateAsset ::
  -- | 'assetId'
  Prelude.Text ->
  -- | 'assetName'
  Prelude.Text ->
  UpdateAsset
newUpdateAsset pAssetId_ pAssetName_ =
  UpdateAsset'
    { assetDescription = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      assetId = pAssetId_,
      assetName = pAssetName_
    }

-- | A description for the asset.
updateAsset_assetDescription :: Lens.Lens' UpdateAsset (Prelude.Maybe Prelude.Text)
updateAsset_assetDescription = Lens.lens (\UpdateAsset' {assetDescription} -> assetDescription) (\s@UpdateAsset' {} a -> s {assetDescription = a} :: UpdateAsset)

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
updateAsset_clientToken :: Lens.Lens' UpdateAsset (Prelude.Maybe Prelude.Text)
updateAsset_clientToken = Lens.lens (\UpdateAsset' {clientToken} -> clientToken) (\s@UpdateAsset' {} a -> s {clientToken = a} :: UpdateAsset)

-- | The ID of the asset to update.
updateAsset_assetId :: Lens.Lens' UpdateAsset Prelude.Text
updateAsset_assetId = Lens.lens (\UpdateAsset' {assetId} -> assetId) (\s@UpdateAsset' {} a -> s {assetId = a} :: UpdateAsset)

-- | A friendly name for the asset.
updateAsset_assetName :: Lens.Lens' UpdateAsset Prelude.Text
updateAsset_assetName = Lens.lens (\UpdateAsset' {assetName} -> assetName) (\s@UpdateAsset' {} a -> s {assetName = a} :: UpdateAsset)

instance Core.AWSRequest UpdateAsset where
  type AWSResponse UpdateAsset = UpdateAssetResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAssetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "assetStatus")
      )

instance Prelude.Hashable UpdateAsset where
  hashWithSalt _salt UpdateAsset' {..} =
    _salt `Prelude.hashWithSalt` assetDescription
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` assetName

instance Prelude.NFData UpdateAsset where
  rnf UpdateAsset' {..} =
    Prelude.rnf assetDescription
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf assetName

instance Data.ToHeaders UpdateAsset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAsset where
  toJSON UpdateAsset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("assetDescription" Data..=)
              Prelude.<$> assetDescription,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("assetName" Data..= assetName)
          ]
      )

instance Data.ToPath UpdateAsset where
  toPath UpdateAsset' {..} =
    Prelude.mconcat ["/assets/", Data.toBS assetId]

instance Data.ToQuery UpdateAsset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAssetResponse' smart constructor.
data UpdateAssetResponse = UpdateAssetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the asset, which contains a state (@UPDATING@ after
    -- successfully calling this operation) and any error message.
    assetStatus :: AssetStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAssetResponse_httpStatus' - The response's http status code.
--
-- 'assetStatus', 'updateAssetResponse_assetStatus' - The status of the asset, which contains a state (@UPDATING@ after
-- successfully calling this operation) and any error message.
newUpdateAssetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'assetStatus'
  AssetStatus ->
  UpdateAssetResponse
newUpdateAssetResponse pHttpStatus_ pAssetStatus_ =
  UpdateAssetResponse'
    { httpStatus = pHttpStatus_,
      assetStatus = pAssetStatus_
    }

-- | The response's http status code.
updateAssetResponse_httpStatus :: Lens.Lens' UpdateAssetResponse Prelude.Int
updateAssetResponse_httpStatus = Lens.lens (\UpdateAssetResponse' {httpStatus} -> httpStatus) (\s@UpdateAssetResponse' {} a -> s {httpStatus = a} :: UpdateAssetResponse)

-- | The status of the asset, which contains a state (@UPDATING@ after
-- successfully calling this operation) and any error message.
updateAssetResponse_assetStatus :: Lens.Lens' UpdateAssetResponse AssetStatus
updateAssetResponse_assetStatus = Lens.lens (\UpdateAssetResponse' {assetStatus} -> assetStatus) (\s@UpdateAssetResponse' {} a -> s {assetStatus = a} :: UpdateAssetResponse)

instance Prelude.NFData UpdateAssetResponse where
  rnf UpdateAssetResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assetStatus
