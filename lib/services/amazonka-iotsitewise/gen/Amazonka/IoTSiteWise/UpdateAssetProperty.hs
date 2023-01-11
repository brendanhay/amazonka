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
-- Module      : Amazonka.IoTSiteWise.UpdateAssetProperty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an asset property\'s alias and notification state.
--
-- This operation overwrites the property\'s existing alias and
-- notification state. To keep your existing property\'s alias or
-- notification state, you must include the existing values in the
-- UpdateAssetProperty request. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_DescribeAssetProperty.html DescribeAssetProperty>.
module Amazonka.IoTSiteWise.UpdateAssetProperty
  ( -- * Creating a Request
    UpdateAssetProperty (..),
    newUpdateAssetProperty,

    -- * Request Lenses
    updateAssetProperty_clientToken,
    updateAssetProperty_propertyAlias,
    updateAssetProperty_propertyNotificationState,
    updateAssetProperty_propertyUnit,
    updateAssetProperty_assetId,
    updateAssetProperty_propertyId,

    -- * Destructuring the Response
    UpdateAssetPropertyResponse (..),
    newUpdateAssetPropertyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAssetProperty' smart constructor.
data UpdateAssetProperty = UpdateAssetProperty'
  { -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The alias that identifies the property, such as an OPC-UA server data
    -- stream path (for example,
    -- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
    -- in the /IoT SiteWise User Guide/.
    --
    -- If you omit this parameter, the alias is removed from the property.
    propertyAlias :: Prelude.Maybe Prelude.Text,
    -- | The MQTT notification state (enabled or disabled) for this asset
    -- property. When the notification state is enabled, IoT SiteWise publishes
    -- property value updates to a unique MQTT topic. For more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/interact-with-other-services.html Interacting with other services>
    -- in the /IoT SiteWise User Guide/.
    --
    -- If you omit this parameter, the notification state is set to @DISABLED@.
    propertyNotificationState :: Prelude.Maybe PropertyNotificationState,
    -- | The unit of measure (such as Newtons or RPM) of the asset property. If
    -- you don\'t specify a value for this parameter, the service uses the
    -- value of the @assetModelProperty@ in the asset model.
    propertyUnit :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset to be updated.
    assetId :: Prelude.Text,
    -- | The ID of the asset property to be updated.
    propertyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssetProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateAssetProperty_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'propertyAlias', 'updateAssetProperty_propertyAlias' - The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
--
-- If you omit this parameter, the alias is removed from the property.
--
-- 'propertyNotificationState', 'updateAssetProperty_propertyNotificationState' - The MQTT notification state (enabled or disabled) for this asset
-- property. When the notification state is enabled, IoT SiteWise publishes
-- property value updates to a unique MQTT topic. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/interact-with-other-services.html Interacting with other services>
-- in the /IoT SiteWise User Guide/.
--
-- If you omit this parameter, the notification state is set to @DISABLED@.
--
-- 'propertyUnit', 'updateAssetProperty_propertyUnit' - The unit of measure (such as Newtons or RPM) of the asset property. If
-- you don\'t specify a value for this parameter, the service uses the
-- value of the @assetModelProperty@ in the asset model.
--
-- 'assetId', 'updateAssetProperty_assetId' - The ID of the asset to be updated.
--
-- 'propertyId', 'updateAssetProperty_propertyId' - The ID of the asset property to be updated.
newUpdateAssetProperty ::
  -- | 'assetId'
  Prelude.Text ->
  -- | 'propertyId'
  Prelude.Text ->
  UpdateAssetProperty
newUpdateAssetProperty pAssetId_ pPropertyId_ =
  UpdateAssetProperty'
    { clientToken = Prelude.Nothing,
      propertyAlias = Prelude.Nothing,
      propertyNotificationState = Prelude.Nothing,
      propertyUnit = Prelude.Nothing,
      assetId = pAssetId_,
      propertyId = pPropertyId_
    }

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
updateAssetProperty_clientToken :: Lens.Lens' UpdateAssetProperty (Prelude.Maybe Prelude.Text)
updateAssetProperty_clientToken = Lens.lens (\UpdateAssetProperty' {clientToken} -> clientToken) (\s@UpdateAssetProperty' {} a -> s {clientToken = a} :: UpdateAssetProperty)

-- | The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
--
-- If you omit this parameter, the alias is removed from the property.
updateAssetProperty_propertyAlias :: Lens.Lens' UpdateAssetProperty (Prelude.Maybe Prelude.Text)
updateAssetProperty_propertyAlias = Lens.lens (\UpdateAssetProperty' {propertyAlias} -> propertyAlias) (\s@UpdateAssetProperty' {} a -> s {propertyAlias = a} :: UpdateAssetProperty)

-- | The MQTT notification state (enabled or disabled) for this asset
-- property. When the notification state is enabled, IoT SiteWise publishes
-- property value updates to a unique MQTT topic. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/interact-with-other-services.html Interacting with other services>
-- in the /IoT SiteWise User Guide/.
--
-- If you omit this parameter, the notification state is set to @DISABLED@.
updateAssetProperty_propertyNotificationState :: Lens.Lens' UpdateAssetProperty (Prelude.Maybe PropertyNotificationState)
updateAssetProperty_propertyNotificationState = Lens.lens (\UpdateAssetProperty' {propertyNotificationState} -> propertyNotificationState) (\s@UpdateAssetProperty' {} a -> s {propertyNotificationState = a} :: UpdateAssetProperty)

-- | The unit of measure (such as Newtons or RPM) of the asset property. If
-- you don\'t specify a value for this parameter, the service uses the
-- value of the @assetModelProperty@ in the asset model.
updateAssetProperty_propertyUnit :: Lens.Lens' UpdateAssetProperty (Prelude.Maybe Prelude.Text)
updateAssetProperty_propertyUnit = Lens.lens (\UpdateAssetProperty' {propertyUnit} -> propertyUnit) (\s@UpdateAssetProperty' {} a -> s {propertyUnit = a} :: UpdateAssetProperty)

-- | The ID of the asset to be updated.
updateAssetProperty_assetId :: Lens.Lens' UpdateAssetProperty Prelude.Text
updateAssetProperty_assetId = Lens.lens (\UpdateAssetProperty' {assetId} -> assetId) (\s@UpdateAssetProperty' {} a -> s {assetId = a} :: UpdateAssetProperty)

-- | The ID of the asset property to be updated.
updateAssetProperty_propertyId :: Lens.Lens' UpdateAssetProperty Prelude.Text
updateAssetProperty_propertyId = Lens.lens (\UpdateAssetProperty' {propertyId} -> propertyId) (\s@UpdateAssetProperty' {} a -> s {propertyId = a} :: UpdateAssetProperty)

instance Core.AWSRequest UpdateAssetProperty where
  type
    AWSResponse UpdateAssetProperty =
      UpdateAssetPropertyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateAssetPropertyResponse'

instance Prelude.Hashable UpdateAssetProperty where
  hashWithSalt _salt UpdateAssetProperty' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` propertyAlias
      `Prelude.hashWithSalt` propertyNotificationState
      `Prelude.hashWithSalt` propertyUnit
      `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` propertyId

instance Prelude.NFData UpdateAssetProperty where
  rnf UpdateAssetProperty' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf propertyAlias
      `Prelude.seq` Prelude.rnf propertyNotificationState
      `Prelude.seq` Prelude.rnf propertyUnit
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf propertyId

instance Data.ToHeaders UpdateAssetProperty where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAssetProperty where
  toJSON UpdateAssetProperty' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("propertyAlias" Data..=) Prelude.<$> propertyAlias,
            ("propertyNotificationState" Data..=)
              Prelude.<$> propertyNotificationState,
            ("propertyUnit" Data..=) Prelude.<$> propertyUnit
          ]
      )

instance Data.ToPath UpdateAssetProperty where
  toPath UpdateAssetProperty' {..} =
    Prelude.mconcat
      [ "/assets/",
        Data.toBS assetId,
        "/properties/",
        Data.toBS propertyId
      ]

instance Data.ToQuery UpdateAssetProperty where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAssetPropertyResponse' smart constructor.
data UpdateAssetPropertyResponse = UpdateAssetPropertyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssetPropertyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateAssetPropertyResponse ::
  UpdateAssetPropertyResponse
newUpdateAssetPropertyResponse =
  UpdateAssetPropertyResponse'

instance Prelude.NFData UpdateAssetPropertyResponse where
  rnf _ = ()
