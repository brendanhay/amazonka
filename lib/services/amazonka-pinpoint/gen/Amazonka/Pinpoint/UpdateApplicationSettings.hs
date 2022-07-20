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
-- Module      : Amazonka.Pinpoint.UpdateApplicationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings for an application.
module Amazonka.Pinpoint.UpdateApplicationSettings
  ( -- * Creating a Request
    UpdateApplicationSettings (..),
    newUpdateApplicationSettings,

    -- * Request Lenses
    updateApplicationSettings_applicationId,
    updateApplicationSettings_writeApplicationSettingsRequest,

    -- * Destructuring the Response
    UpdateApplicationSettingsResponse (..),
    newUpdateApplicationSettingsResponse,

    -- * Response Lenses
    updateApplicationSettingsResponse_httpStatus,
    updateApplicationSettingsResponse_applicationSettingsResource,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApplicationSettings' smart constructor.
data UpdateApplicationSettings = UpdateApplicationSettings'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    writeApplicationSettingsRequest :: WriteApplicationSettingsRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplicationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateApplicationSettings_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'writeApplicationSettingsRequest', 'updateApplicationSettings_writeApplicationSettingsRequest' - Undocumented member.
newUpdateApplicationSettings ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'writeApplicationSettingsRequest'
  WriteApplicationSettingsRequest ->
  UpdateApplicationSettings
newUpdateApplicationSettings
  pApplicationId_
  pWriteApplicationSettingsRequest_ =
    UpdateApplicationSettings'
      { applicationId =
          pApplicationId_,
        writeApplicationSettingsRequest =
          pWriteApplicationSettingsRequest_
      }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
updateApplicationSettings_applicationId :: Lens.Lens' UpdateApplicationSettings Prelude.Text
updateApplicationSettings_applicationId = Lens.lens (\UpdateApplicationSettings' {applicationId} -> applicationId) (\s@UpdateApplicationSettings' {} a -> s {applicationId = a} :: UpdateApplicationSettings)

-- | Undocumented member.
updateApplicationSettings_writeApplicationSettingsRequest :: Lens.Lens' UpdateApplicationSettings WriteApplicationSettingsRequest
updateApplicationSettings_writeApplicationSettingsRequest = Lens.lens (\UpdateApplicationSettings' {writeApplicationSettingsRequest} -> writeApplicationSettingsRequest) (\s@UpdateApplicationSettings' {} a -> s {writeApplicationSettingsRequest = a} :: UpdateApplicationSettings)

instance Core.AWSRequest UpdateApplicationSettings where
  type
    AWSResponse UpdateApplicationSettings =
      UpdateApplicationSettingsResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApplicationSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable UpdateApplicationSettings where
  hashWithSalt _salt UpdateApplicationSettings' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` writeApplicationSettingsRequest

instance Prelude.NFData UpdateApplicationSettings where
  rnf UpdateApplicationSettings' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf writeApplicationSettingsRequest

instance Core.ToHeaders UpdateApplicationSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateApplicationSettings where
  toJSON UpdateApplicationSettings' {..} =
    Core.toJSON writeApplicationSettingsRequest

instance Core.ToPath UpdateApplicationSettings where
  toPath UpdateApplicationSettings' {..} =
    Prelude.mconcat
      ["/v1/apps/", Core.toBS applicationId, "/settings"]

instance Core.ToQuery UpdateApplicationSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApplicationSettingsResponse' smart constructor.
data UpdateApplicationSettingsResponse = UpdateApplicationSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    applicationSettingsResource :: ApplicationSettingsResource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplicationSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateApplicationSettingsResponse_httpStatus' - The response's http status code.
--
-- 'applicationSettingsResource', 'updateApplicationSettingsResponse_applicationSettingsResource' - Undocumented member.
newUpdateApplicationSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationSettingsResource'
  ApplicationSettingsResource ->
  UpdateApplicationSettingsResponse
newUpdateApplicationSettingsResponse
  pHttpStatus_
  pApplicationSettingsResource_ =
    UpdateApplicationSettingsResponse'
      { httpStatus =
          pHttpStatus_,
        applicationSettingsResource =
          pApplicationSettingsResource_
      }

-- | The response's http status code.
updateApplicationSettingsResponse_httpStatus :: Lens.Lens' UpdateApplicationSettingsResponse Prelude.Int
updateApplicationSettingsResponse_httpStatus = Lens.lens (\UpdateApplicationSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateApplicationSettingsResponse' {} a -> s {httpStatus = a} :: UpdateApplicationSettingsResponse)

-- | Undocumented member.
updateApplicationSettingsResponse_applicationSettingsResource :: Lens.Lens' UpdateApplicationSettingsResponse ApplicationSettingsResource
updateApplicationSettingsResponse_applicationSettingsResource = Lens.lens (\UpdateApplicationSettingsResponse' {applicationSettingsResource} -> applicationSettingsResource) (\s@UpdateApplicationSettingsResponse' {} a -> s {applicationSettingsResource = a} :: UpdateApplicationSettingsResponse)

instance
  Prelude.NFData
    UpdateApplicationSettingsResponse
  where
  rnf UpdateApplicationSettingsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationSettingsResource
