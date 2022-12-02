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
-- Module      : Amazonka.WorkSpacesWeb.GetBrowserSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets browser settings.
module Amazonka.WorkSpacesWeb.GetBrowserSettings
  ( -- * Creating a Request
    GetBrowserSettings (..),
    newGetBrowserSettings,

    -- * Request Lenses
    getBrowserSettings_browserSettingsArn,

    -- * Destructuring the Response
    GetBrowserSettingsResponse (..),
    newGetBrowserSettingsResponse,

    -- * Response Lenses
    getBrowserSettingsResponse_browserSettings,
    getBrowserSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newGetBrowserSettings' smart constructor.
data GetBrowserSettings = GetBrowserSettings'
  { -- | The ARN of the browser settings.
    browserSettingsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBrowserSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'browserSettingsArn', 'getBrowserSettings_browserSettingsArn' - The ARN of the browser settings.
newGetBrowserSettings ::
  -- | 'browserSettingsArn'
  Prelude.Text ->
  GetBrowserSettings
newGetBrowserSettings pBrowserSettingsArn_ =
  GetBrowserSettings'
    { browserSettingsArn =
        pBrowserSettingsArn_
    }

-- | The ARN of the browser settings.
getBrowserSettings_browserSettingsArn :: Lens.Lens' GetBrowserSettings Prelude.Text
getBrowserSettings_browserSettingsArn = Lens.lens (\GetBrowserSettings' {browserSettingsArn} -> browserSettingsArn) (\s@GetBrowserSettings' {} a -> s {browserSettingsArn = a} :: GetBrowserSettings)

instance Core.AWSRequest GetBrowserSettings where
  type
    AWSResponse GetBrowserSettings =
      GetBrowserSettingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBrowserSettingsResponse'
            Prelude.<$> (x Data..?> "browserSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBrowserSettings where
  hashWithSalt _salt GetBrowserSettings' {..} =
    _salt `Prelude.hashWithSalt` browserSettingsArn

instance Prelude.NFData GetBrowserSettings where
  rnf GetBrowserSettings' {..} =
    Prelude.rnf browserSettingsArn

instance Data.ToHeaders GetBrowserSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetBrowserSettings where
  toPath GetBrowserSettings' {..} =
    Prelude.mconcat
      ["/browserSettings/", Data.toBS browserSettingsArn]

instance Data.ToQuery GetBrowserSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBrowserSettingsResponse' smart constructor.
data GetBrowserSettingsResponse = GetBrowserSettingsResponse'
  { -- | The browser settings.
    browserSettings :: Prelude.Maybe BrowserSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBrowserSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'browserSettings', 'getBrowserSettingsResponse_browserSettings' - The browser settings.
--
-- 'httpStatus', 'getBrowserSettingsResponse_httpStatus' - The response's http status code.
newGetBrowserSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBrowserSettingsResponse
newGetBrowserSettingsResponse pHttpStatus_ =
  GetBrowserSettingsResponse'
    { browserSettings =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The browser settings.
getBrowserSettingsResponse_browserSettings :: Lens.Lens' GetBrowserSettingsResponse (Prelude.Maybe BrowserSettings)
getBrowserSettingsResponse_browserSettings = Lens.lens (\GetBrowserSettingsResponse' {browserSettings} -> browserSettings) (\s@GetBrowserSettingsResponse' {} a -> s {browserSettings = a} :: GetBrowserSettingsResponse)

-- | The response's http status code.
getBrowserSettingsResponse_httpStatus :: Lens.Lens' GetBrowserSettingsResponse Prelude.Int
getBrowserSettingsResponse_httpStatus = Lens.lens (\GetBrowserSettingsResponse' {httpStatus} -> httpStatus) (\s@GetBrowserSettingsResponse' {} a -> s {httpStatus = a} :: GetBrowserSettingsResponse)

instance Prelude.NFData GetBrowserSettingsResponse where
  rnf GetBrowserSettingsResponse' {..} =
    Prelude.rnf browserSettings
      `Prelude.seq` Prelude.rnf httpStatus
