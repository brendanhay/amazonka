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
-- Module      : Network.AWS.GuardDuty.GetMasterAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the details for the GuardDuty administrator account associated
-- with the current GuardDuty member account.
module Network.AWS.GuardDuty.GetMasterAccount
  ( -- * Creating a Request
    GetMasterAccount (..),
    newGetMasterAccount,

    -- * Request Lenses
    getMasterAccount_detectorId,

    -- * Destructuring the Response
    GetMasterAccountResponse (..),
    newGetMasterAccountResponse,

    -- * Response Lenses
    getMasterAccountResponse_httpStatus,
    getMasterAccountResponse_master,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMasterAccount' smart constructor.
data GetMasterAccount = GetMasterAccount'
  { -- | The unique ID of the detector of the GuardDuty member account.
    detectorId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMasterAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'getMasterAccount_detectorId' - The unique ID of the detector of the GuardDuty member account.
newGetMasterAccount ::
  -- | 'detectorId'
  Core.Text ->
  GetMasterAccount
newGetMasterAccount pDetectorId_ =
  GetMasterAccount' {detectorId = pDetectorId_}

-- | The unique ID of the detector of the GuardDuty member account.
getMasterAccount_detectorId :: Lens.Lens' GetMasterAccount Core.Text
getMasterAccount_detectorId = Lens.lens (\GetMasterAccount' {detectorId} -> detectorId) (\s@GetMasterAccount' {} a -> s {detectorId = a} :: GetMasterAccount)

instance Core.AWSRequest GetMasterAccount where
  type
    AWSResponse GetMasterAccount =
      GetMasterAccountResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMasterAccountResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "master")
      )

instance Core.Hashable GetMasterAccount

instance Core.NFData GetMasterAccount

instance Core.ToHeaders GetMasterAccount where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetMasterAccount where
  toPath GetMasterAccount' {..} =
    Core.mconcat
      ["/detector/", Core.toBS detectorId, "/master"]

instance Core.ToQuery GetMasterAccount where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetMasterAccountResponse' smart constructor.
data GetMasterAccountResponse = GetMasterAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The administrator account details.
    master :: Master
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetMasterAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getMasterAccountResponse_httpStatus' - The response's http status code.
--
-- 'master', 'getMasterAccountResponse_master' - The administrator account details.
newGetMasterAccountResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'master'
  Master ->
  GetMasterAccountResponse
newGetMasterAccountResponse pHttpStatus_ pMaster_ =
  GetMasterAccountResponse'
    { httpStatus =
        pHttpStatus_,
      master = pMaster_
    }

-- | The response's http status code.
getMasterAccountResponse_httpStatus :: Lens.Lens' GetMasterAccountResponse Core.Int
getMasterAccountResponse_httpStatus = Lens.lens (\GetMasterAccountResponse' {httpStatus} -> httpStatus) (\s@GetMasterAccountResponse' {} a -> s {httpStatus = a} :: GetMasterAccountResponse)

-- | The administrator account details.
getMasterAccountResponse_master :: Lens.Lens' GetMasterAccountResponse Master
getMasterAccountResponse_master = Lens.lens (\GetMasterAccountResponse' {master} -> master) (\s@GetMasterAccountResponse' {} a -> s {master = a} :: GetMasterAccountResponse)

instance Core.NFData GetMasterAccountResponse
