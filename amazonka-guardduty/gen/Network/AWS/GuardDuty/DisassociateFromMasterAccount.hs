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
-- Module      : Network.AWS.GuardDuty.DisassociateFromMasterAccount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the current GuardDuty member account from its
-- administrator account.
module Network.AWS.GuardDuty.DisassociateFromMasterAccount
  ( -- * Creating a Request
    DisassociateFromMasterAccount (..),
    newDisassociateFromMasterAccount,

    -- * Request Lenses
    disassociateFromMasterAccount_detectorId,

    -- * Destructuring the Response
    DisassociateFromMasterAccountResponse (..),
    newDisassociateFromMasterAccountResponse,

    -- * Response Lenses
    disassociateFromMasterAccountResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateFromMasterAccount' smart constructor.
data DisassociateFromMasterAccount = DisassociateFromMasterAccount'
  { -- | The unique ID of the detector of the GuardDuty member account.
    detectorId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateFromMasterAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'disassociateFromMasterAccount_detectorId' - The unique ID of the detector of the GuardDuty member account.
newDisassociateFromMasterAccount ::
  -- | 'detectorId'
  Core.Text ->
  DisassociateFromMasterAccount
newDisassociateFromMasterAccount pDetectorId_ =
  DisassociateFromMasterAccount'
    { detectorId =
        pDetectorId_
    }

-- | The unique ID of the detector of the GuardDuty member account.
disassociateFromMasterAccount_detectorId :: Lens.Lens' DisassociateFromMasterAccount Core.Text
disassociateFromMasterAccount_detectorId = Lens.lens (\DisassociateFromMasterAccount' {detectorId} -> detectorId) (\s@DisassociateFromMasterAccount' {} a -> s {detectorId = a} :: DisassociateFromMasterAccount)

instance
  Core.AWSRequest
    DisassociateFromMasterAccount
  where
  type
    AWSResponse DisassociateFromMasterAccount =
      DisassociateFromMasterAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateFromMasterAccountResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisassociateFromMasterAccount

instance Core.NFData DisassociateFromMasterAccount

instance Core.ToHeaders DisassociateFromMasterAccount where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisassociateFromMasterAccount where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DisassociateFromMasterAccount where
  toPath DisassociateFromMasterAccount' {..} =
    Core.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/master/disassociate"
      ]

instance Core.ToQuery DisassociateFromMasterAccount where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateFromMasterAccountResponse' smart constructor.
data DisassociateFromMasterAccountResponse = DisassociateFromMasterAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateFromMasterAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateFromMasterAccountResponse_httpStatus' - The response's http status code.
newDisassociateFromMasterAccountResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisassociateFromMasterAccountResponse
newDisassociateFromMasterAccountResponse pHttpStatus_ =
  DisassociateFromMasterAccountResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateFromMasterAccountResponse_httpStatus :: Lens.Lens' DisassociateFromMasterAccountResponse Core.Int
disassociateFromMasterAccountResponse_httpStatus = Lens.lens (\DisassociateFromMasterAccountResponse' {httpStatus} -> httpStatus) (\s@DisassociateFromMasterAccountResponse' {} a -> s {httpStatus = a} :: DisassociateFromMasterAccountResponse)

instance
  Core.NFData
    DisassociateFromMasterAccountResponse
