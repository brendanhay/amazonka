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
-- Module      : Amazonka.AuditManager.GetSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the settings for a specified Amazon Web Services account.
module Amazonka.AuditManager.GetSettings
  ( -- * Creating a Request
    GetSettings (..),
    newGetSettings,

    -- * Request Lenses
    getSettings_attribute,

    -- * Destructuring the Response
    GetSettingsResponse (..),
    newGetSettingsResponse,

    -- * Response Lenses
    getSettingsResponse_settings,
    getSettingsResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSettings' smart constructor.
data GetSettings = GetSettings'
  { -- | The list of setting attribute enum values.
    attribute :: SettingAttribute
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'getSettings_attribute' - The list of setting attribute enum values.
newGetSettings ::
  -- | 'attribute'
  SettingAttribute ->
  GetSettings
newGetSettings pAttribute_ =
  GetSettings' {attribute = pAttribute_}

-- | The list of setting attribute enum values.
getSettings_attribute :: Lens.Lens' GetSettings SettingAttribute
getSettings_attribute = Lens.lens (\GetSettings' {attribute} -> attribute) (\s@GetSettings' {} a -> s {attribute = a} :: GetSettings)

instance Core.AWSRequest GetSettings where
  type AWSResponse GetSettings = GetSettingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSettingsResponse'
            Prelude.<$> (x Data..?> "settings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSettings where
  hashWithSalt _salt GetSettings' {..} =
    _salt `Prelude.hashWithSalt` attribute

instance Prelude.NFData GetSettings where
  rnf GetSettings' {..} = Prelude.rnf attribute

instance Data.ToHeaders GetSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSettings where
  toPath GetSettings' {..} =
    Prelude.mconcat ["/settings/", Data.toBS attribute]

instance Data.ToQuery GetSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSettingsResponse' smart constructor.
data GetSettingsResponse = GetSettingsResponse'
  { -- | The settings object that holds all supported Audit Manager settings.
    settings :: Prelude.Maybe Settings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settings', 'getSettingsResponse_settings' - The settings object that holds all supported Audit Manager settings.
--
-- 'httpStatus', 'getSettingsResponse_httpStatus' - The response's http status code.
newGetSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSettingsResponse
newGetSettingsResponse pHttpStatus_ =
  GetSettingsResponse'
    { settings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The settings object that holds all supported Audit Manager settings.
getSettingsResponse_settings :: Lens.Lens' GetSettingsResponse (Prelude.Maybe Settings)
getSettingsResponse_settings = Lens.lens (\GetSettingsResponse' {settings} -> settings) (\s@GetSettingsResponse' {} a -> s {settings = a} :: GetSettingsResponse)

-- | The response's http status code.
getSettingsResponse_httpStatus :: Lens.Lens' GetSettingsResponse Prelude.Int
getSettingsResponse_httpStatus = Lens.lens (\GetSettingsResponse' {httpStatus} -> httpStatus) (\s@GetSettingsResponse' {} a -> s {httpStatus = a} :: GetSettingsResponse)

instance Prelude.NFData GetSettingsResponse where
  rnf GetSettingsResponse' {..} =
    Prelude.rnf settings
      `Prelude.seq` Prelude.rnf httpStatus
