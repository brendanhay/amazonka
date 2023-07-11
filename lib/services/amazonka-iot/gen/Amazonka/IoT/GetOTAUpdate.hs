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
-- Module      : Amazonka.IoT.GetOTAUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an OTA update.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetOTAUpdate>
-- action.
module Amazonka.IoT.GetOTAUpdate
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOTAUpdateResponse'
            Prelude.<$> (x Data..?> "otaUpdateInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOTAUpdate where
  hashWithSalt _salt GetOTAUpdate' {..} =
    _salt `Prelude.hashWithSalt` otaUpdateId

instance Prelude.NFData GetOTAUpdate where
  rnf GetOTAUpdate' {..} = Prelude.rnf otaUpdateId

instance Data.ToHeaders GetOTAUpdate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetOTAUpdate where
  toPath GetOTAUpdate' {..} =
    Prelude.mconcat
      ["/otaUpdates/", Data.toBS otaUpdateId]

instance Data.ToQuery GetOTAUpdate where
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

instance Prelude.NFData GetOTAUpdateResponse where
  rnf GetOTAUpdateResponse' {..} =
    Prelude.rnf otaUpdateInfo
      `Prelude.seq` Prelude.rnf httpStatus
