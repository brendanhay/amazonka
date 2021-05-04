{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Greengrass.UpdateConnectivityInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the connectivity information for the core. Any devices that
-- belong to the group which has this core will receive this information in
-- order to find the location of the core and connect to it.
module Network.AWS.Greengrass.UpdateConnectivityInfo
  ( -- * Creating a Request
    UpdateConnectivityInfo (..),
    newUpdateConnectivityInfo,

    -- * Request Lenses
    updateConnectivityInfo_connectivityInfo,
    updateConnectivityInfo_thingName,

    -- * Destructuring the Response
    UpdateConnectivityInfoResponse (..),
    newUpdateConnectivityInfoResponse,

    -- * Response Lenses
    updateConnectivityInfoResponse_message,
    updateConnectivityInfoResponse_version,
    updateConnectivityInfoResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Connectivity information.
--
-- /See:/ 'newUpdateConnectivityInfo' smart constructor.
data UpdateConnectivityInfo = UpdateConnectivityInfo'
  { -- | A list of connectivity info.
    connectivityInfo :: Prelude.Maybe [ConnectivityInfo],
    -- | The thing name.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectivityInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectivityInfo', 'updateConnectivityInfo_connectivityInfo' - A list of connectivity info.
--
-- 'thingName', 'updateConnectivityInfo_thingName' - The thing name.
newUpdateConnectivityInfo ::
  -- | 'thingName'
  Prelude.Text ->
  UpdateConnectivityInfo
newUpdateConnectivityInfo pThingName_ =
  UpdateConnectivityInfo'
    { connectivityInfo =
        Prelude.Nothing,
      thingName = pThingName_
    }

-- | A list of connectivity info.
updateConnectivityInfo_connectivityInfo :: Lens.Lens' UpdateConnectivityInfo (Prelude.Maybe [ConnectivityInfo])
updateConnectivityInfo_connectivityInfo = Lens.lens (\UpdateConnectivityInfo' {connectivityInfo} -> connectivityInfo) (\s@UpdateConnectivityInfo' {} a -> s {connectivityInfo = a} :: UpdateConnectivityInfo) Prelude.. Lens.mapping Prelude._Coerce

-- | The thing name.
updateConnectivityInfo_thingName :: Lens.Lens' UpdateConnectivityInfo Prelude.Text
updateConnectivityInfo_thingName = Lens.lens (\UpdateConnectivityInfo' {thingName} -> thingName) (\s@UpdateConnectivityInfo' {} a -> s {thingName = a} :: UpdateConnectivityInfo)

instance Prelude.AWSRequest UpdateConnectivityInfo where
  type
    Rs UpdateConnectivityInfo =
      UpdateConnectivityInfoResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateConnectivityInfoResponse'
            Prelude.<$> (x Prelude..?> "message")
            Prelude.<*> (x Prelude..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateConnectivityInfo

instance Prelude.NFData UpdateConnectivityInfo

instance Prelude.ToHeaders UpdateConnectivityInfo where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateConnectivityInfo where
  toJSON UpdateConnectivityInfo' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ConnectivityInfo" Prelude..=)
              Prelude.<$> connectivityInfo
          ]
      )

instance Prelude.ToPath UpdateConnectivityInfo where
  toPath UpdateConnectivityInfo' {..} =
    Prelude.mconcat
      [ "/greengrass/things/",
        Prelude.toBS thingName,
        "/connectivityInfo"
      ]

instance Prelude.ToQuery UpdateConnectivityInfo where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConnectivityInfoResponse' smart constructor.
data UpdateConnectivityInfoResponse = UpdateConnectivityInfoResponse'
  { -- | A message about the connectivity info update request.
    message :: Prelude.Maybe Prelude.Text,
    -- | The new version of the connectivity info.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectivityInfoResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'updateConnectivityInfoResponse_message' - A message about the connectivity info update request.
--
-- 'version', 'updateConnectivityInfoResponse_version' - The new version of the connectivity info.
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

-- | A message about the connectivity info update request.
updateConnectivityInfoResponse_message :: Lens.Lens' UpdateConnectivityInfoResponse (Prelude.Maybe Prelude.Text)
updateConnectivityInfoResponse_message = Lens.lens (\UpdateConnectivityInfoResponse' {message} -> message) (\s@UpdateConnectivityInfoResponse' {} a -> s {message = a} :: UpdateConnectivityInfoResponse)

-- | The new version of the connectivity info.
updateConnectivityInfoResponse_version :: Lens.Lens' UpdateConnectivityInfoResponse (Prelude.Maybe Prelude.Text)
updateConnectivityInfoResponse_version = Lens.lens (\UpdateConnectivityInfoResponse' {version} -> version) (\s@UpdateConnectivityInfoResponse' {} a -> s {version = a} :: UpdateConnectivityInfoResponse)

-- | The response's http status code.
updateConnectivityInfoResponse_httpStatus :: Lens.Lens' UpdateConnectivityInfoResponse Prelude.Int
updateConnectivityInfoResponse_httpStatus = Lens.lens (\UpdateConnectivityInfoResponse' {httpStatus} -> httpStatus) (\s@UpdateConnectivityInfoResponse' {} a -> s {httpStatus = a} :: UpdateConnectivityInfoResponse)

instance
  Prelude.NFData
    UpdateConnectivityInfoResponse
