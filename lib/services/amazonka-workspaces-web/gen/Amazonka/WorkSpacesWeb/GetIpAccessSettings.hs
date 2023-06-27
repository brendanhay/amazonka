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
-- Module      : Amazonka.WorkSpacesWeb.GetIpAccessSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the IP access settings.
module Amazonka.WorkSpacesWeb.GetIpAccessSettings
  ( -- * Creating a Request
    GetIpAccessSettings (..),
    newGetIpAccessSettings,

    -- * Request Lenses
    getIpAccessSettings_ipAccessSettingsArn,

    -- * Destructuring the Response
    GetIpAccessSettingsResponse (..),
    newGetIpAccessSettingsResponse,

    -- * Response Lenses
    getIpAccessSettingsResponse_ipAccessSettings,
    getIpAccessSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newGetIpAccessSettings' smart constructor.
data GetIpAccessSettings = GetIpAccessSettings'
  { -- | The ARN of the IP access settings.
    ipAccessSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIpAccessSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAccessSettingsArn', 'getIpAccessSettings_ipAccessSettingsArn' - The ARN of the IP access settings.
newGetIpAccessSettings ::
  -- | 'ipAccessSettingsArn'
  Prelude.Text ->
  GetIpAccessSettings
newGetIpAccessSettings pIpAccessSettingsArn_ =
  GetIpAccessSettings'
    { ipAccessSettingsArn =
        pIpAccessSettingsArn_
    }

-- | The ARN of the IP access settings.
getIpAccessSettings_ipAccessSettingsArn :: Lens.Lens' GetIpAccessSettings Prelude.Text
getIpAccessSettings_ipAccessSettingsArn = Lens.lens (\GetIpAccessSettings' {ipAccessSettingsArn} -> ipAccessSettingsArn) (\s@GetIpAccessSettings' {} a -> s {ipAccessSettingsArn = a} :: GetIpAccessSettings)

instance Core.AWSRequest GetIpAccessSettings where
  type
    AWSResponse GetIpAccessSettings =
      GetIpAccessSettingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIpAccessSettingsResponse'
            Prelude.<$> (x Data..?> "ipAccessSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIpAccessSettings where
  hashWithSalt _salt GetIpAccessSettings' {..} =
    _salt `Prelude.hashWithSalt` ipAccessSettingsArn

instance Prelude.NFData GetIpAccessSettings where
  rnf GetIpAccessSettings' {..} =
    Prelude.rnf ipAccessSettingsArn

instance Data.ToHeaders GetIpAccessSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetIpAccessSettings where
  toPath GetIpAccessSettings' {..} =
    Prelude.mconcat
      ["/ipAccessSettings/", Data.toBS ipAccessSettingsArn]

instance Data.ToQuery GetIpAccessSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetIpAccessSettingsResponse' smart constructor.
data GetIpAccessSettingsResponse = GetIpAccessSettingsResponse'
  { -- | The IP access settings.
    ipAccessSettings :: Prelude.Maybe IpAccessSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIpAccessSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAccessSettings', 'getIpAccessSettingsResponse_ipAccessSettings' - The IP access settings.
--
-- 'httpStatus', 'getIpAccessSettingsResponse_httpStatus' - The response's http status code.
newGetIpAccessSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIpAccessSettingsResponse
newGetIpAccessSettingsResponse pHttpStatus_ =
  GetIpAccessSettingsResponse'
    { ipAccessSettings =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The IP access settings.
getIpAccessSettingsResponse_ipAccessSettings :: Lens.Lens' GetIpAccessSettingsResponse (Prelude.Maybe IpAccessSettings)
getIpAccessSettingsResponse_ipAccessSettings = Lens.lens (\GetIpAccessSettingsResponse' {ipAccessSettings} -> ipAccessSettings) (\s@GetIpAccessSettingsResponse' {} a -> s {ipAccessSettings = a} :: GetIpAccessSettingsResponse)

-- | The response's http status code.
getIpAccessSettingsResponse_httpStatus :: Lens.Lens' GetIpAccessSettingsResponse Prelude.Int
getIpAccessSettingsResponse_httpStatus = Lens.lens (\GetIpAccessSettingsResponse' {httpStatus} -> httpStatus) (\s@GetIpAccessSettingsResponse' {} a -> s {httpStatus = a} :: GetIpAccessSettingsResponse)

instance Prelude.NFData GetIpAccessSettingsResponse where
  rnf GetIpAccessSettingsResponse' {..} =
    Prelude.rnf ipAccessSettings
      `Prelude.seq` Prelude.rnf httpStatus
