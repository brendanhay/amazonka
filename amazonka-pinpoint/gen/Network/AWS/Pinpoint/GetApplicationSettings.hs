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
-- Module      : Network.AWS.Pinpoint.GetApplicationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the settings for an application.
module Network.AWS.Pinpoint.GetApplicationSettings
  ( -- * Creating a Request
    GetApplicationSettings (..),
    newGetApplicationSettings,

    -- * Request Lenses
    getApplicationSettings_applicationId,

    -- * Destructuring the Response
    GetApplicationSettingsResponse (..),
    newGetApplicationSettingsResponse,

    -- * Response Lenses
    getApplicationSettingsResponse_httpStatus,
    getApplicationSettingsResponse_applicationSettingsResource,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetApplicationSettings' smart constructor.
data GetApplicationSettings = GetApplicationSettings'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplicationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getApplicationSettings_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetApplicationSettings ::
  -- | 'applicationId'
  Prelude.Text ->
  GetApplicationSettings
newGetApplicationSettings pApplicationId_ =
  GetApplicationSettings'
    { applicationId =
        pApplicationId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getApplicationSettings_applicationId :: Lens.Lens' GetApplicationSettings Prelude.Text
getApplicationSettings_applicationId = Lens.lens (\GetApplicationSettings' {applicationId} -> applicationId) (\s@GetApplicationSettings' {} a -> s {applicationId = a} :: GetApplicationSettings)

instance Core.AWSRequest GetApplicationSettings where
  type
    AWSResponse GetApplicationSettings =
      GetApplicationSettingsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable GetApplicationSettings

instance Prelude.NFData GetApplicationSettings

instance Core.ToHeaders GetApplicationSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetApplicationSettings where
  toPath GetApplicationSettings' {..} =
    Prelude.mconcat
      ["/v1/apps/", Core.toBS applicationId, "/settings"]

instance Core.ToQuery GetApplicationSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApplicationSettingsResponse' smart constructor.
data GetApplicationSettingsResponse = GetApplicationSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    applicationSettingsResource :: ApplicationSettingsResource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplicationSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getApplicationSettingsResponse_httpStatus' - The response's http status code.
--
-- 'applicationSettingsResource', 'getApplicationSettingsResponse_applicationSettingsResource' - Undocumented member.
newGetApplicationSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationSettingsResource'
  ApplicationSettingsResource ->
  GetApplicationSettingsResponse
newGetApplicationSettingsResponse
  pHttpStatus_
  pApplicationSettingsResource_ =
    GetApplicationSettingsResponse'
      { httpStatus =
          pHttpStatus_,
        applicationSettingsResource =
          pApplicationSettingsResource_
      }

-- | The response's http status code.
getApplicationSettingsResponse_httpStatus :: Lens.Lens' GetApplicationSettingsResponse Prelude.Int
getApplicationSettingsResponse_httpStatus = Lens.lens (\GetApplicationSettingsResponse' {httpStatus} -> httpStatus) (\s@GetApplicationSettingsResponse' {} a -> s {httpStatus = a} :: GetApplicationSettingsResponse)

-- | Undocumented member.
getApplicationSettingsResponse_applicationSettingsResource :: Lens.Lens' GetApplicationSettingsResponse ApplicationSettingsResource
getApplicationSettingsResponse_applicationSettingsResource = Lens.lens (\GetApplicationSettingsResponse' {applicationSettingsResource} -> applicationSettingsResource) (\s@GetApplicationSettingsResponse' {} a -> s {applicationSettingsResource = a} :: GetApplicationSettingsResponse)

instance
  Prelude.NFData
    GetApplicationSettingsResponse
