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
-- Module      : Amazonka.WorkSpacesWeb.GetUserAccessLoggingSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets user access logging settings.
module Amazonka.WorkSpacesWeb.GetUserAccessLoggingSettings
  ( -- * Creating a Request
    GetUserAccessLoggingSettings (..),
    newGetUserAccessLoggingSettings,

    -- * Request Lenses
    getUserAccessLoggingSettings_userAccessLoggingSettingsArn,

    -- * Destructuring the Response
    GetUserAccessLoggingSettingsResponse (..),
    newGetUserAccessLoggingSettingsResponse,

    -- * Response Lenses
    getUserAccessLoggingSettingsResponse_userAccessLoggingSettings,
    getUserAccessLoggingSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newGetUserAccessLoggingSettings' smart constructor.
data GetUserAccessLoggingSettings = GetUserAccessLoggingSettings'
  { -- | The ARN of the user access logging settings.
    userAccessLoggingSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUserAccessLoggingSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userAccessLoggingSettingsArn', 'getUserAccessLoggingSettings_userAccessLoggingSettingsArn' - The ARN of the user access logging settings.
newGetUserAccessLoggingSettings ::
  -- | 'userAccessLoggingSettingsArn'
  Prelude.Text ->
  GetUserAccessLoggingSettings
newGetUserAccessLoggingSettings
  pUserAccessLoggingSettingsArn_ =
    GetUserAccessLoggingSettings'
      { userAccessLoggingSettingsArn =
          pUserAccessLoggingSettingsArn_
      }

-- | The ARN of the user access logging settings.
getUserAccessLoggingSettings_userAccessLoggingSettingsArn :: Lens.Lens' GetUserAccessLoggingSettings Prelude.Text
getUserAccessLoggingSettings_userAccessLoggingSettingsArn = Lens.lens (\GetUserAccessLoggingSettings' {userAccessLoggingSettingsArn} -> userAccessLoggingSettingsArn) (\s@GetUserAccessLoggingSettings' {} a -> s {userAccessLoggingSettingsArn = a} :: GetUserAccessLoggingSettings)

instance Core.AWSRequest GetUserAccessLoggingSettings where
  type
    AWSResponse GetUserAccessLoggingSettings =
      GetUserAccessLoggingSettingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUserAccessLoggingSettingsResponse'
            Prelude.<$> (x Data..?> "userAccessLoggingSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetUserAccessLoggingSettings
  where
  hashWithSalt _salt GetUserAccessLoggingSettings' {..} =
    _salt
      `Prelude.hashWithSalt` userAccessLoggingSettingsArn

instance Prelude.NFData GetUserAccessLoggingSettings where
  rnf GetUserAccessLoggingSettings' {..} =
    Prelude.rnf userAccessLoggingSettingsArn

instance Data.ToHeaders GetUserAccessLoggingSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetUserAccessLoggingSettings where
  toPath GetUserAccessLoggingSettings' {..} =
    Prelude.mconcat
      [ "/userAccessLoggingSettings/",
        Data.toBS userAccessLoggingSettingsArn
      ]

instance Data.ToQuery GetUserAccessLoggingSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetUserAccessLoggingSettingsResponse' smart constructor.
data GetUserAccessLoggingSettingsResponse = GetUserAccessLoggingSettingsResponse'
  { -- | The user access logging settings.
    userAccessLoggingSettings :: Prelude.Maybe UserAccessLoggingSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUserAccessLoggingSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userAccessLoggingSettings', 'getUserAccessLoggingSettingsResponse_userAccessLoggingSettings' - The user access logging settings.
--
-- 'httpStatus', 'getUserAccessLoggingSettingsResponse_httpStatus' - The response's http status code.
newGetUserAccessLoggingSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetUserAccessLoggingSettingsResponse
newGetUserAccessLoggingSettingsResponse pHttpStatus_ =
  GetUserAccessLoggingSettingsResponse'
    { userAccessLoggingSettings =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user access logging settings.
getUserAccessLoggingSettingsResponse_userAccessLoggingSettings :: Lens.Lens' GetUserAccessLoggingSettingsResponse (Prelude.Maybe UserAccessLoggingSettings)
getUserAccessLoggingSettingsResponse_userAccessLoggingSettings = Lens.lens (\GetUserAccessLoggingSettingsResponse' {userAccessLoggingSettings} -> userAccessLoggingSettings) (\s@GetUserAccessLoggingSettingsResponse' {} a -> s {userAccessLoggingSettings = a} :: GetUserAccessLoggingSettingsResponse)

-- | The response's http status code.
getUserAccessLoggingSettingsResponse_httpStatus :: Lens.Lens' GetUserAccessLoggingSettingsResponse Prelude.Int
getUserAccessLoggingSettingsResponse_httpStatus = Lens.lens (\GetUserAccessLoggingSettingsResponse' {httpStatus} -> httpStatus) (\s@GetUserAccessLoggingSettingsResponse' {} a -> s {httpStatus = a} :: GetUserAccessLoggingSettingsResponse)

instance
  Prelude.NFData
    GetUserAccessLoggingSettingsResponse
  where
  rnf GetUserAccessLoggingSettingsResponse' {..} =
    Prelude.rnf userAccessLoggingSettings
      `Prelude.seq` Prelude.rnf httpStatus
