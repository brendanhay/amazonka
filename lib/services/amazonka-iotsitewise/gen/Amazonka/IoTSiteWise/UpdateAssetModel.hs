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
-- Module      : Amazonka.IoTSiteWise.UpdateAssetModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an asset model and all of the assets that were created from the
-- model. Each asset created from the model inherits the updated asset
-- model\'s property and hierarchy definitions. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/update-assets-and-models.html Updating assets and models>
-- in the /IoT SiteWise User Guide/.
--
-- This operation overwrites the existing model with the provided model. To
-- avoid deleting your asset model\'s properties or hierarchies, you must
-- include their IDs and definitions in the updated asset model payload.
-- For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_DescribeAssetModel.html DescribeAssetModel>.
--
-- If you remove a property from an asset model, IoT SiteWise deletes all
-- previous data for that property. If you remove a hierarchy definition
-- from an asset model, IoT SiteWise disassociates every asset associated
-- with that hierarchy. You can\'t change the type or data type of an
-- existing property.
module Amazonka.IoTSiteWise.UpdateAssetModel
  ( -- * Creating a Request
    UpdateAssetModel (..),
    newUpdateAssetModel,

    -- * Request Lenses
    updateAssetModel_assetModelCompositeModels,
    updateAssetModel_assetModelDescription,
    updateAssetModel_assetModelHierarchies,
    updateAssetModel_assetModelProperties,
    updateAssetModel_clientToken,
    updateAssetModel_assetModelId,
    updateAssetModel_assetModelName,

    -- * Destructuring the Response
    UpdateAssetModelResponse (..),
    newUpdateAssetModelResponse,

    -- * Response Lenses
    updateAssetModelResponse_httpStatus,
    updateAssetModelResponse_assetModelStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAssetModel' smart constructor.
data UpdateAssetModel = UpdateAssetModel'
  { -- | The composite asset models that are part of this asset model. Composite
    -- asset models are asset models that contain specific properties. Each
    -- composite model has a type that defines the properties that the
    -- composite model supports. Use composite asset models to define alarms on
    -- this asset model.
    assetModelCompositeModels :: Prelude.Maybe [AssetModelCompositeModel],
    -- | A description for the asset model.
    assetModelDescription :: Prelude.Maybe Prelude.Text,
    -- | The updated hierarchy definitions of the asset model. Each hierarchy
    -- specifies an asset model whose assets can be children of any other
    -- assets created from this asset model. For more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-hierarchies.html Asset hierarchies>
    -- in the /IoT SiteWise User Guide/.
    --
    -- You can specify up to 10 hierarchies per asset model. For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
    -- in the /IoT SiteWise User Guide/.
    assetModelHierarchies :: Prelude.Maybe [AssetModelHierarchy],
    -- | The updated property definitions of the asset model. For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-properties.html Asset properties>
    -- in the /IoT SiteWise User Guide/.
    --
    -- You can specify up to 200 properties per asset model. For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
    -- in the /IoT SiteWise User Guide/.
    assetModelProperties :: Prelude.Maybe [AssetModelProperty],
    -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset model to update.
    assetModelId :: Prelude.Text,
    -- | A unique, friendly name for the asset model.
    assetModelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssetModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetModelCompositeModels', 'updateAssetModel_assetModelCompositeModels' - The composite asset models that are part of this asset model. Composite
-- asset models are asset models that contain specific properties. Each
-- composite model has a type that defines the properties that the
-- composite model supports. Use composite asset models to define alarms on
-- this asset model.
--
-- 'assetModelDescription', 'updateAssetModel_assetModelDescription' - A description for the asset model.
--
-- 'assetModelHierarchies', 'updateAssetModel_assetModelHierarchies' - The updated hierarchy definitions of the asset model. Each hierarchy
-- specifies an asset model whose assets can be children of any other
-- assets created from this asset model. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-hierarchies.html Asset hierarchies>
-- in the /IoT SiteWise User Guide/.
--
-- You can specify up to 10 hierarchies per asset model. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
-- in the /IoT SiteWise User Guide/.
--
-- 'assetModelProperties', 'updateAssetModel_assetModelProperties' - The updated property definitions of the asset model. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-properties.html Asset properties>
-- in the /IoT SiteWise User Guide/.
--
-- You can specify up to 200 properties per asset model. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
-- in the /IoT SiteWise User Guide/.
--
-- 'clientToken', 'updateAssetModel_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'assetModelId', 'updateAssetModel_assetModelId' - The ID of the asset model to update.
--
-- 'assetModelName', 'updateAssetModel_assetModelName' - A unique, friendly name for the asset model.
newUpdateAssetModel ::
  -- | 'assetModelId'
  Prelude.Text ->
  -- | 'assetModelName'
  Prelude.Text ->
  UpdateAssetModel
newUpdateAssetModel pAssetModelId_ pAssetModelName_ =
  UpdateAssetModel'
    { assetModelCompositeModels =
        Prelude.Nothing,
      assetModelDescription = Prelude.Nothing,
      assetModelHierarchies = Prelude.Nothing,
      assetModelProperties = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      assetModelId = pAssetModelId_,
      assetModelName = pAssetModelName_
    }

-- | The composite asset models that are part of this asset model. Composite
-- asset models are asset models that contain specific properties. Each
-- composite model has a type that defines the properties that the
-- composite model supports. Use composite asset models to define alarms on
-- this asset model.
updateAssetModel_assetModelCompositeModels :: Lens.Lens' UpdateAssetModel (Prelude.Maybe [AssetModelCompositeModel])
updateAssetModel_assetModelCompositeModels = Lens.lens (\UpdateAssetModel' {assetModelCompositeModels} -> assetModelCompositeModels) (\s@UpdateAssetModel' {} a -> s {assetModelCompositeModels = a} :: UpdateAssetModel) Prelude.. Lens.mapping Lens.coerced

-- | A description for the asset model.
updateAssetModel_assetModelDescription :: Lens.Lens' UpdateAssetModel (Prelude.Maybe Prelude.Text)
updateAssetModel_assetModelDescription = Lens.lens (\UpdateAssetModel' {assetModelDescription} -> assetModelDescription) (\s@UpdateAssetModel' {} a -> s {assetModelDescription = a} :: UpdateAssetModel)

-- | The updated hierarchy definitions of the asset model. Each hierarchy
-- specifies an asset model whose assets can be children of any other
-- assets created from this asset model. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-hierarchies.html Asset hierarchies>
-- in the /IoT SiteWise User Guide/.
--
-- You can specify up to 10 hierarchies per asset model. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
-- in the /IoT SiteWise User Guide/.
updateAssetModel_assetModelHierarchies :: Lens.Lens' UpdateAssetModel (Prelude.Maybe [AssetModelHierarchy])
updateAssetModel_assetModelHierarchies = Lens.lens (\UpdateAssetModel' {assetModelHierarchies} -> assetModelHierarchies) (\s@UpdateAssetModel' {} a -> s {assetModelHierarchies = a} :: UpdateAssetModel) Prelude.. Lens.mapping Lens.coerced

-- | The updated property definitions of the asset model. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-properties.html Asset properties>
-- in the /IoT SiteWise User Guide/.
--
-- You can specify up to 200 properties per asset model. For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/quotas.html Quotas>
-- in the /IoT SiteWise User Guide/.
updateAssetModel_assetModelProperties :: Lens.Lens' UpdateAssetModel (Prelude.Maybe [AssetModelProperty])
updateAssetModel_assetModelProperties = Lens.lens (\UpdateAssetModel' {assetModelProperties} -> assetModelProperties) (\s@UpdateAssetModel' {} a -> s {assetModelProperties = a} :: UpdateAssetModel) Prelude.. Lens.mapping Lens.coerced

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
updateAssetModel_clientToken :: Lens.Lens' UpdateAssetModel (Prelude.Maybe Prelude.Text)
updateAssetModel_clientToken = Lens.lens (\UpdateAssetModel' {clientToken} -> clientToken) (\s@UpdateAssetModel' {} a -> s {clientToken = a} :: UpdateAssetModel)

-- | The ID of the asset model to update.
updateAssetModel_assetModelId :: Lens.Lens' UpdateAssetModel Prelude.Text
updateAssetModel_assetModelId = Lens.lens (\UpdateAssetModel' {assetModelId} -> assetModelId) (\s@UpdateAssetModel' {} a -> s {assetModelId = a} :: UpdateAssetModel)

-- | A unique, friendly name for the asset model.
updateAssetModel_assetModelName :: Lens.Lens' UpdateAssetModel Prelude.Text
updateAssetModel_assetModelName = Lens.lens (\UpdateAssetModel' {assetModelName} -> assetModelName) (\s@UpdateAssetModel' {} a -> s {assetModelName = a} :: UpdateAssetModel)

instance Core.AWSRequest UpdateAssetModel where
  type
    AWSResponse UpdateAssetModel =
      UpdateAssetModelResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAssetModelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "assetModelStatus")
      )

instance Prelude.Hashable UpdateAssetModel where
  hashWithSalt _salt UpdateAssetModel' {..} =
    _salt
      `Prelude.hashWithSalt` assetModelCompositeModels
      `Prelude.hashWithSalt` assetModelDescription
      `Prelude.hashWithSalt` assetModelHierarchies
      `Prelude.hashWithSalt` assetModelProperties
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` assetModelId
      `Prelude.hashWithSalt` assetModelName

instance Prelude.NFData UpdateAssetModel where
  rnf UpdateAssetModel' {..} =
    Prelude.rnf assetModelCompositeModels `Prelude.seq`
      Prelude.rnf assetModelDescription `Prelude.seq`
        Prelude.rnf assetModelHierarchies `Prelude.seq`
          Prelude.rnf assetModelProperties `Prelude.seq`
            Prelude.rnf clientToken `Prelude.seq`
              Prelude.rnf assetModelId `Prelude.seq`
                Prelude.rnf assetModelName

instance Data.ToHeaders UpdateAssetModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAssetModel where
  toJSON UpdateAssetModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("assetModelCompositeModels" Data..=)
              Prelude.<$> assetModelCompositeModels,
            ("assetModelDescription" Data..=)
              Prelude.<$> assetModelDescription,
            ("assetModelHierarchies" Data..=)
              Prelude.<$> assetModelHierarchies,
            ("assetModelProperties" Data..=)
              Prelude.<$> assetModelProperties,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just
              ("assetModelName" Data..= assetModelName)
          ]
      )

instance Data.ToPath UpdateAssetModel where
  toPath UpdateAssetModel' {..} =
    Prelude.mconcat
      ["/asset-models/", Data.toBS assetModelId]

instance Data.ToQuery UpdateAssetModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAssetModelResponse' smart constructor.
data UpdateAssetModelResponse = UpdateAssetModelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the asset model, which contains a state (@UPDATING@ after
    -- successfully calling this operation) and any error message.
    assetModelStatus :: AssetModelStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssetModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAssetModelResponse_httpStatus' - The response's http status code.
--
-- 'assetModelStatus', 'updateAssetModelResponse_assetModelStatus' - The status of the asset model, which contains a state (@UPDATING@ after
-- successfully calling this operation) and any error message.
newUpdateAssetModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'assetModelStatus'
  AssetModelStatus ->
  UpdateAssetModelResponse
newUpdateAssetModelResponse
  pHttpStatus_
  pAssetModelStatus_ =
    UpdateAssetModelResponse'
      { httpStatus =
          pHttpStatus_,
        assetModelStatus = pAssetModelStatus_
      }

-- | The response's http status code.
updateAssetModelResponse_httpStatus :: Lens.Lens' UpdateAssetModelResponse Prelude.Int
updateAssetModelResponse_httpStatus = Lens.lens (\UpdateAssetModelResponse' {httpStatus} -> httpStatus) (\s@UpdateAssetModelResponse' {} a -> s {httpStatus = a} :: UpdateAssetModelResponse)

-- | The status of the asset model, which contains a state (@UPDATING@ after
-- successfully calling this operation) and any error message.
updateAssetModelResponse_assetModelStatus :: Lens.Lens' UpdateAssetModelResponse AssetModelStatus
updateAssetModelResponse_assetModelStatus = Lens.lens (\UpdateAssetModelResponse' {assetModelStatus} -> assetModelStatus) (\s@UpdateAssetModelResponse' {} a -> s {assetModelStatus = a} :: UpdateAssetModelResponse)

instance Prelude.NFData UpdateAssetModelResponse where
  rnf UpdateAssetModelResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf assetModelStatus
