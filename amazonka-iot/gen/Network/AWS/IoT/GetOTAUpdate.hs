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
-- Module      : Network.AWS.IoT.GetOTAUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an OTA update.
module Network.AWS.IoT.GetOTAUpdate
  ( -- * Creating a Request
    GetOTAUpdate (..),
    newGetOTAUpdate,

    -- * Request Lenses
    getOTAUpdate_otaUpdateId,

    -- * Destructuring the Response
    GetOTAUpdateResponse (..),
    newGetOTAUpdateResponse,

    -- * Response Lenses
    getOTAUpdateResponse_otaUpdateInfo,
    getOTAUpdateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetOTAUpdate' smart constructor.
data GetOTAUpdate = GetOTAUpdate'
  { -- | The OTA update ID.
    otaUpdateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOTAUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'otaUpdateId', 'getOTAUpdate_otaUpdateId' - The OTA update ID.
newGetOTAUpdate ::
  -- | 'otaUpdateId'
  Prelude.Text ->
  GetOTAUpdate
newGetOTAUpdate pOtaUpdateId_ =
  GetOTAUpdate' {otaUpdateId = pOtaUpdateId_}

-- | The OTA update ID.
getOTAUpdate_otaUpdateId :: Lens.Lens' GetOTAUpdate Prelude.Text
getOTAUpdate_otaUpdateId = Lens.lens (\GetOTAUpdate' {otaUpdateId} -> otaUpdateId) (\s@GetOTAUpdate' {} a -> s {otaUpdateId = a} :: GetOTAUpdate)

instance Core.AWSRequest GetOTAUpdate where
  type AWSResponse GetOTAUpdate = GetOTAUpdateResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOTAUpdateResponse'
            Prelude.<$> (x Core..?> "otaUpdateInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOTAUpdate

instance Prelude.NFData GetOTAUpdate

instance Core.ToHeaders GetOTAUpdate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetOTAUpdate where
  toPath GetOTAUpdate' {..} =
    Prelude.mconcat
      ["/otaUpdates/", Core.toBS otaUpdateId]

instance Core.ToQuery GetOTAUpdate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOTAUpdateResponse' smart constructor.
data GetOTAUpdateResponse = GetOTAUpdateResponse'
  { -- | The OTA update info.
    otaUpdateInfo :: Prelude.Maybe OTAUpdateInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOTAUpdateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'otaUpdateInfo', 'getOTAUpdateResponse_otaUpdateInfo' - The OTA update info.
--
-- 'httpStatus', 'getOTAUpdateResponse_httpStatus' - The response's http status code.
newGetOTAUpdateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOTAUpdateResponse
newGetOTAUpdateResponse pHttpStatus_ =
  GetOTAUpdateResponse'
    { otaUpdateInfo =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The OTA update info.
getOTAUpdateResponse_otaUpdateInfo :: Lens.Lens' GetOTAUpdateResponse (Prelude.Maybe OTAUpdateInfo)
getOTAUpdateResponse_otaUpdateInfo = Lens.lens (\GetOTAUpdateResponse' {otaUpdateInfo} -> otaUpdateInfo) (\s@GetOTAUpdateResponse' {} a -> s {otaUpdateInfo = a} :: GetOTAUpdateResponse)

-- | The response's http status code.
getOTAUpdateResponse_httpStatus :: Lens.Lens' GetOTAUpdateResponse Prelude.Int
getOTAUpdateResponse_httpStatus = Lens.lens (\GetOTAUpdateResponse' {httpStatus} -> httpStatus) (\s@GetOTAUpdateResponse' {} a -> s {httpStatus = a} :: GetOTAUpdateResponse)

instance Prelude.NFData GetOTAUpdateResponse
