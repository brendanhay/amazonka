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
-- Module      : Amazonka.DirectoryService.UpdateSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configurable settings for the specified directory.
module Amazonka.DirectoryService.UpdateSettings
  ( -- * Creating a Request
    UpdateSettings (..),
    newUpdateSettings,

    -- * Request Lenses
    updateSettings_directoryId,
    updateSettings_settings,

    -- * Destructuring the Response
    UpdateSettingsResponse (..),
    newUpdateSettingsResponse,

    -- * Response Lenses
    updateSettingsResponse_directoryId,
    updateSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSettings' smart constructor.
data UpdateSettings = UpdateSettings'
  { -- | The identifier of the directory for which to update settings.
    directoryId :: Prelude.Text,
    -- | The list of Setting objects.
    settings :: [Setting]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'updateSettings_directoryId' - The identifier of the directory for which to update settings.
--
-- 'settings', 'updateSettings_settings' - The list of Setting objects.
newUpdateSettings ::
  -- | 'directoryId'
  Prelude.Text ->
  UpdateSettings
newUpdateSettings pDirectoryId_ =
  UpdateSettings'
    { directoryId = pDirectoryId_,
      settings = Prelude.mempty
    }

-- | The identifier of the directory for which to update settings.
updateSettings_directoryId :: Lens.Lens' UpdateSettings Prelude.Text
updateSettings_directoryId = Lens.lens (\UpdateSettings' {directoryId} -> directoryId) (\s@UpdateSettings' {} a -> s {directoryId = a} :: UpdateSettings)

-- | The list of Setting objects.
updateSettings_settings :: Lens.Lens' UpdateSettings [Setting]
updateSettings_settings = Lens.lens (\UpdateSettings' {settings} -> settings) (\s@UpdateSettings' {} a -> s {settings = a} :: UpdateSettings) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateSettings where
  type
    AWSResponse UpdateSettings =
      UpdateSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSettingsResponse'
            Prelude.<$> (x Core..?> "DirectoryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSettings where
  hashWithSalt _salt UpdateSettings' {..} =
    _salt `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` settings

instance Prelude.NFData UpdateSettings where
  rnf UpdateSettings' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf settings

instance Core.ToHeaders UpdateSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.UpdateSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSettings where
  toJSON UpdateSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Core..= directoryId),
            Prelude.Just ("Settings" Core..= settings)
          ]
      )

instance Core.ToPath UpdateSettings where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSettingsResponse' smart constructor.
data UpdateSettingsResponse = UpdateSettingsResponse'
  { -- | The identifier of the directory.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'updateSettingsResponse_directoryId' - The identifier of the directory.
--
-- 'httpStatus', 'updateSettingsResponse_httpStatus' - The response's http status code.
newUpdateSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSettingsResponse
newUpdateSettingsResponse pHttpStatus_ =
  UpdateSettingsResponse'
    { directoryId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the directory.
updateSettingsResponse_directoryId :: Lens.Lens' UpdateSettingsResponse (Prelude.Maybe Prelude.Text)
updateSettingsResponse_directoryId = Lens.lens (\UpdateSettingsResponse' {directoryId} -> directoryId) (\s@UpdateSettingsResponse' {} a -> s {directoryId = a} :: UpdateSettingsResponse)

-- | The response's http status code.
updateSettingsResponse_httpStatus :: Lens.Lens' UpdateSettingsResponse Prelude.Int
updateSettingsResponse_httpStatus = Lens.lens (\UpdateSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateSettingsResponse' {} a -> s {httpStatus = a} :: UpdateSettingsResponse)

instance Prelude.NFData UpdateSettingsResponse where
  rnf UpdateSettingsResponse' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf httpStatus
