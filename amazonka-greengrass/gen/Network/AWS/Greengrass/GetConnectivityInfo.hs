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
-- Module      : Network.AWS.Greengrass.GetConnectivityInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the connectivity information for a core.
module Network.AWS.Greengrass.GetConnectivityInfo
  ( -- * Creating a Request
    GetConnectivityInfo (..),
    newGetConnectivityInfo,

    -- * Request Lenses
    getConnectivityInfo_thingName,

    -- * Destructuring the Response
    GetConnectivityInfoResponse (..),
    newGetConnectivityInfoResponse,

    -- * Response Lenses
    getConnectivityInfoResponse_message,
    getConnectivityInfoResponse_connectivityInfo,
    getConnectivityInfoResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetConnectivityInfo' smart constructor.
data GetConnectivityInfo = GetConnectivityInfo'
  { -- | The thing name.
    thingName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConnectivityInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingName', 'getConnectivityInfo_thingName' - The thing name.
newGetConnectivityInfo ::
  -- | 'thingName'
  Core.Text ->
  GetConnectivityInfo
newGetConnectivityInfo pThingName_ =
  GetConnectivityInfo' {thingName = pThingName_}

-- | The thing name.
getConnectivityInfo_thingName :: Lens.Lens' GetConnectivityInfo Core.Text
getConnectivityInfo_thingName = Lens.lens (\GetConnectivityInfo' {thingName} -> thingName) (\s@GetConnectivityInfo' {} a -> s {thingName = a} :: GetConnectivityInfo)

instance Core.AWSRequest GetConnectivityInfo where
  type
    AWSResponse GetConnectivityInfo =
      GetConnectivityInfoResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectivityInfoResponse'
            Core.<$> (x Core..?> "message")
            Core.<*> (x Core..?> "ConnectivityInfo" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetConnectivityInfo

instance Core.NFData GetConnectivityInfo

instance Core.ToHeaders GetConnectivityInfo where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetConnectivityInfo where
  toPath GetConnectivityInfo' {..} =
    Core.mconcat
      [ "/greengrass/things/",
        Core.toBS thingName,
        "/connectivityInfo"
      ]

instance Core.ToQuery GetConnectivityInfo where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetConnectivityInfoResponse' smart constructor.
data GetConnectivityInfoResponse = GetConnectivityInfoResponse'
  { -- | A message about the connectivity info request.
    message :: Core.Maybe Core.Text,
    -- | Connectivity info list.
    connectivityInfo :: Core.Maybe [ConnectivityInfo],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConnectivityInfoResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'getConnectivityInfoResponse_message' - A message about the connectivity info request.
--
-- 'connectivityInfo', 'getConnectivityInfoResponse_connectivityInfo' - Connectivity info list.
--
-- 'httpStatus', 'getConnectivityInfoResponse_httpStatus' - The response's http status code.
newGetConnectivityInfoResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetConnectivityInfoResponse
newGetConnectivityInfoResponse pHttpStatus_ =
  GetConnectivityInfoResponse'
    { message =
        Core.Nothing,
      connectivityInfo = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A message about the connectivity info request.
getConnectivityInfoResponse_message :: Lens.Lens' GetConnectivityInfoResponse (Core.Maybe Core.Text)
getConnectivityInfoResponse_message = Lens.lens (\GetConnectivityInfoResponse' {message} -> message) (\s@GetConnectivityInfoResponse' {} a -> s {message = a} :: GetConnectivityInfoResponse)

-- | Connectivity info list.
getConnectivityInfoResponse_connectivityInfo :: Lens.Lens' GetConnectivityInfoResponse (Core.Maybe [ConnectivityInfo])
getConnectivityInfoResponse_connectivityInfo = Lens.lens (\GetConnectivityInfoResponse' {connectivityInfo} -> connectivityInfo) (\s@GetConnectivityInfoResponse' {} a -> s {connectivityInfo = a} :: GetConnectivityInfoResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getConnectivityInfoResponse_httpStatus :: Lens.Lens' GetConnectivityInfoResponse Core.Int
getConnectivityInfoResponse_httpStatus = Lens.lens (\GetConnectivityInfoResponse' {httpStatus} -> httpStatus) (\s@GetConnectivityInfoResponse' {} a -> s {httpStatus = a} :: GetConnectivityInfoResponse)

instance Core.NFData GetConnectivityInfoResponse
