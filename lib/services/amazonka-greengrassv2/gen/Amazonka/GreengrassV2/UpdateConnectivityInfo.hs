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
-- Module      : Amazonka.GreengrassV2.UpdateConnectivityInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates connectivity information for a Greengrass core device.
--
-- Connectivity information includes endpoints and ports where client
-- devices can connect to an MQTT broker on the core device. When a client
-- device calls the
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/greengrass-discover-api.html IoT Greengrass discovery API>,
-- IoT Greengrass returns connectivity information for all of the core
-- devices where the client device can connect. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/connect-client-devices.html Connect client devices to core devices>
-- in the /IoT Greengrass Version 2 Developer Guide/.
module Amazonka.GreengrassV2.UpdateConnectivityInfo
  ( -- * Creating a Request
    UpdateConnectivityInfo (..),
    newUpdateConnectivityInfo,

    -- * Request Lenses
    updateConnectivityInfo_thingName,
    updateConnectivityInfo_connectivityInfo,

    -- * Destructuring the Response
    UpdateConnectivityInfoResponse (..),
    newUpdateConnectivityInfoResponse,

    -- * Response Lenses
    updateConnectivityInfoResponse_message,
    updateConnectivityInfoResponse_version,
    updateConnectivityInfoResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateConnectivityInfo' smart constructor.
data UpdateConnectivityInfo = UpdateConnectivityInfo'
  { -- | The name of the core device. This is also the name of the IoT thing.
    thingName :: Prelude.Text,
    -- | The connectivity information for the core device.
    connectivityInfo :: [ConnectivityInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectivityInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingName', 'updateConnectivityInfo_thingName' - The name of the core device. This is also the name of the IoT thing.
--
-- 'connectivityInfo', 'updateConnectivityInfo_connectivityInfo' - The connectivity information for the core device.
newUpdateConnectivityInfo ::
  -- | 'thingName'
  Prelude.Text ->
  UpdateConnectivityInfo
newUpdateConnectivityInfo pThingName_ =
  UpdateConnectivityInfo'
    { thingName = pThingName_,
      connectivityInfo = Prelude.mempty
    }

-- | The name of the core device. This is also the name of the IoT thing.
updateConnectivityInfo_thingName :: Lens.Lens' UpdateConnectivityInfo Prelude.Text
updateConnectivityInfo_thingName = Lens.lens (\UpdateConnectivityInfo' {thingName} -> thingName) (\s@UpdateConnectivityInfo' {} a -> s {thingName = a} :: UpdateConnectivityInfo)

-- | The connectivity information for the core device.
updateConnectivityInfo_connectivityInfo :: Lens.Lens' UpdateConnectivityInfo [ConnectivityInfo]
updateConnectivityInfo_connectivityInfo = Lens.lens (\UpdateConnectivityInfo' {connectivityInfo} -> connectivityInfo) (\s@UpdateConnectivityInfo' {} a -> s {connectivityInfo = a} :: UpdateConnectivityInfo) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateConnectivityInfo where
  type
    AWSResponse UpdateConnectivityInfo =
      UpdateConnectivityInfoResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateConnectivityInfoResponse'
            Prelude.<$> (x Core..?> "Message")
            Prelude.<*> (x Core..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateConnectivityInfo where
  hashWithSalt _salt UpdateConnectivityInfo' {..} =
    _salt `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` connectivityInfo

instance Prelude.NFData UpdateConnectivityInfo where
  rnf UpdateConnectivityInfo' {..} =
    Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf connectivityInfo

instance Core.ToHeaders UpdateConnectivityInfo where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateConnectivityInfo where
  toJSON UpdateConnectivityInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ConnectivityInfo" Core..= connectivityInfo)
          ]
      )

instance Core.ToPath UpdateConnectivityInfo where
  toPath UpdateConnectivityInfo' {..} =
    Prelude.mconcat
      [ "/greengrass/things/",
        Core.toBS thingName,
        "/connectivityInfo"
      ]

instance Core.ToQuery UpdateConnectivityInfo where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConnectivityInfoResponse' smart constructor.
data UpdateConnectivityInfoResponse = UpdateConnectivityInfoResponse'
  { -- | A message about the connectivity information update request.
    message :: Prelude.Maybe Prelude.Text,
    -- | The new version of the connectivity information for the core device.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectivityInfoResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'updateConnectivityInfoResponse_message' - A message about the connectivity information update request.
--
-- 'version', 'updateConnectivityInfoResponse_version' - The new version of the connectivity information for the core device.
--
-- 'httpStatus', 'updateConnectivityInfoResponse_httpStatus' - The response's http status code.
newUpdateConnectivityInfoResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateConnectivityInfoResponse
newUpdateConnectivityInfoResponse pHttpStatus_ =
  UpdateConnectivityInfoResponse'
    { message =
        Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A message about the connectivity information update request.
updateConnectivityInfoResponse_message :: Lens.Lens' UpdateConnectivityInfoResponse (Prelude.Maybe Prelude.Text)
updateConnectivityInfoResponse_message = Lens.lens (\UpdateConnectivityInfoResponse' {message} -> message) (\s@UpdateConnectivityInfoResponse' {} a -> s {message = a} :: UpdateConnectivityInfoResponse)

-- | The new version of the connectivity information for the core device.
updateConnectivityInfoResponse_version :: Lens.Lens' UpdateConnectivityInfoResponse (Prelude.Maybe Prelude.Text)
updateConnectivityInfoResponse_version = Lens.lens (\UpdateConnectivityInfoResponse' {version} -> version) (\s@UpdateConnectivityInfoResponse' {} a -> s {version = a} :: UpdateConnectivityInfoResponse)

-- | The response's http status code.
updateConnectivityInfoResponse_httpStatus :: Lens.Lens' UpdateConnectivityInfoResponse Prelude.Int
updateConnectivityInfoResponse_httpStatus = Lens.lens (\UpdateConnectivityInfoResponse' {httpStatus} -> httpStatus) (\s@UpdateConnectivityInfoResponse' {} a -> s {httpStatus = a} :: UpdateConnectivityInfoResponse)

instance
  Prelude.NFData
    UpdateConnectivityInfoResponse
  where
  rnf UpdateConnectivityInfoResponse' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
