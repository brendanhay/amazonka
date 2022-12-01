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
-- Module      : Amazonka.Greengrass.GetConnectivityInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the connectivity information for a core.
module Amazonka.Greengrass.GetConnectivityInfo
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConnectivityInfo' smart constructor.
data GetConnectivityInfo = GetConnectivityInfo'
  { -- | The thing name.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetConnectivityInfo
newGetConnectivityInfo pThingName_ =
  GetConnectivityInfo' {thingName = pThingName_}

-- | The thing name.
getConnectivityInfo_thingName :: Lens.Lens' GetConnectivityInfo Prelude.Text
getConnectivityInfo_thingName = Lens.lens (\GetConnectivityInfo' {thingName} -> thingName) (\s@GetConnectivityInfo' {} a -> s {thingName = a} :: GetConnectivityInfo)

instance Core.AWSRequest GetConnectivityInfo where
  type
    AWSResponse GetConnectivityInfo =
      GetConnectivityInfoResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectivityInfoResponse'
            Prelude.<$> (x Core..?> "message")
            Prelude.<*> ( x Core..?> "ConnectivityInfo"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConnectivityInfo where
  hashWithSalt _salt GetConnectivityInfo' {..} =
    _salt `Prelude.hashWithSalt` thingName

instance Prelude.NFData GetConnectivityInfo where
  rnf GetConnectivityInfo' {..} = Prelude.rnf thingName

instance Core.ToHeaders GetConnectivityInfo where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetConnectivityInfo where
  toPath GetConnectivityInfo' {..} =
    Prelude.mconcat
      [ "/greengrass/things/",
        Core.toBS thingName,
        "/connectivityInfo"
      ]

instance Core.ToQuery GetConnectivityInfo where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConnectivityInfoResponse' smart constructor.
data GetConnectivityInfoResponse = GetConnectivityInfoResponse'
  { -- | A message about the connectivity info request.
    message :: Prelude.Maybe Prelude.Text,
    -- | Connectivity info list.
    connectivityInfo :: Prelude.Maybe [ConnectivityInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetConnectivityInfoResponse
newGetConnectivityInfoResponse pHttpStatus_ =
  GetConnectivityInfoResponse'
    { message =
        Prelude.Nothing,
      connectivityInfo = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A message about the connectivity info request.
getConnectivityInfoResponse_message :: Lens.Lens' GetConnectivityInfoResponse (Prelude.Maybe Prelude.Text)
getConnectivityInfoResponse_message = Lens.lens (\GetConnectivityInfoResponse' {message} -> message) (\s@GetConnectivityInfoResponse' {} a -> s {message = a} :: GetConnectivityInfoResponse)

-- | Connectivity info list.
getConnectivityInfoResponse_connectivityInfo :: Lens.Lens' GetConnectivityInfoResponse (Prelude.Maybe [ConnectivityInfo])
getConnectivityInfoResponse_connectivityInfo = Lens.lens (\GetConnectivityInfoResponse' {connectivityInfo} -> connectivityInfo) (\s@GetConnectivityInfoResponse' {} a -> s {connectivityInfo = a} :: GetConnectivityInfoResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getConnectivityInfoResponse_httpStatus :: Lens.Lens' GetConnectivityInfoResponse Prelude.Int
getConnectivityInfoResponse_httpStatus = Lens.lens (\GetConnectivityInfoResponse' {httpStatus} -> httpStatus) (\s@GetConnectivityInfoResponse' {} a -> s {httpStatus = a} :: GetConnectivityInfoResponse)

instance Prelude.NFData GetConnectivityInfoResponse where
  rnf GetConnectivityInfoResponse' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf connectivityInfo
      `Prelude.seq` Prelude.rnf httpStatus
