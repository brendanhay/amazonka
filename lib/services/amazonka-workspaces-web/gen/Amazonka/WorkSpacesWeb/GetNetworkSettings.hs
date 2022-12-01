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
-- Module      : Amazonka.WorkSpacesWeb.GetNetworkSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the network settings.
module Amazonka.WorkSpacesWeb.GetNetworkSettings
  ( -- * Creating a Request
    GetNetworkSettings (..),
    newGetNetworkSettings,

    -- * Request Lenses
    getNetworkSettings_networkSettingsArn,

    -- * Destructuring the Response
    GetNetworkSettingsResponse (..),
    newGetNetworkSettingsResponse,

    -- * Response Lenses
    getNetworkSettingsResponse_networkSettings,
    getNetworkSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newGetNetworkSettings' smart constructor.
data GetNetworkSettings = GetNetworkSettings'
  { -- | The ARN of the network settings.
    networkSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkSettingsArn', 'getNetworkSettings_networkSettingsArn' - The ARN of the network settings.
newGetNetworkSettings ::
  -- | 'networkSettingsArn'
  Prelude.Text ->
  GetNetworkSettings
newGetNetworkSettings pNetworkSettingsArn_ =
  GetNetworkSettings'
    { networkSettingsArn =
        pNetworkSettingsArn_
    }

-- | The ARN of the network settings.
getNetworkSettings_networkSettingsArn :: Lens.Lens' GetNetworkSettings Prelude.Text
getNetworkSettings_networkSettingsArn = Lens.lens (\GetNetworkSettings' {networkSettingsArn} -> networkSettingsArn) (\s@GetNetworkSettings' {} a -> s {networkSettingsArn = a} :: GetNetworkSettings)

instance Core.AWSRequest GetNetworkSettings where
  type
    AWSResponse GetNetworkSettings =
      GetNetworkSettingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNetworkSettingsResponse'
            Prelude.<$> (x Core..?> "networkSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetNetworkSettings where
  hashWithSalt _salt GetNetworkSettings' {..} =
    _salt `Prelude.hashWithSalt` networkSettingsArn

instance Prelude.NFData GetNetworkSettings where
  rnf GetNetworkSettings' {..} =
    Prelude.rnf networkSettingsArn

instance Core.ToHeaders GetNetworkSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetNetworkSettings where
  toPath GetNetworkSettings' {..} =
    Prelude.mconcat
      ["/networkSettings/", Core.toBS networkSettingsArn]

instance Core.ToQuery GetNetworkSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetNetworkSettingsResponse' smart constructor.
data GetNetworkSettingsResponse = GetNetworkSettingsResponse'
  { -- | The network settings.
    networkSettings :: Prelude.Maybe NetworkSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkSettings', 'getNetworkSettingsResponse_networkSettings' - The network settings.
--
-- 'httpStatus', 'getNetworkSettingsResponse_httpStatus' - The response's http status code.
newGetNetworkSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetNetworkSettingsResponse
newGetNetworkSettingsResponse pHttpStatus_ =
  GetNetworkSettingsResponse'
    { networkSettings =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The network settings.
getNetworkSettingsResponse_networkSettings :: Lens.Lens' GetNetworkSettingsResponse (Prelude.Maybe NetworkSettings)
getNetworkSettingsResponse_networkSettings = Lens.lens (\GetNetworkSettingsResponse' {networkSettings} -> networkSettings) (\s@GetNetworkSettingsResponse' {} a -> s {networkSettings = a} :: GetNetworkSettingsResponse)

-- | The response's http status code.
getNetworkSettingsResponse_httpStatus :: Lens.Lens' GetNetworkSettingsResponse Prelude.Int
getNetworkSettingsResponse_httpStatus = Lens.lens (\GetNetworkSettingsResponse' {httpStatus} -> httpStatus) (\s@GetNetworkSettingsResponse' {} a -> s {httpStatus = a} :: GetNetworkSettingsResponse)

instance Prelude.NFData GetNetworkSettingsResponse where
  rnf GetNetworkSettingsResponse' {..} =
    Prelude.rnf networkSettings
      `Prelude.seq` Prelude.rnf httpStatus
