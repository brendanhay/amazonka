{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateFromMasterAccount' smart constructor.
data DisassociateFromMasterAccount = DisassociateFromMasterAccount'
  { -- | The unique ID of the detector of the GuardDuty member account.
    detectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DisassociateFromMasterAccount
newDisassociateFromMasterAccount pDetectorId_ =
  DisassociateFromMasterAccount'
    { detectorId =
        pDetectorId_
    }

-- | The unique ID of the detector of the GuardDuty member account.
disassociateFromMasterAccount_detectorId :: Lens.Lens' DisassociateFromMasterAccount Prelude.Text
disassociateFromMasterAccount_detectorId = Lens.lens (\DisassociateFromMasterAccount' {detectorId} -> detectorId) (\s@DisassociateFromMasterAccount' {} a -> s {detectorId = a} :: DisassociateFromMasterAccount)

instance
  Prelude.AWSRequest
    DisassociateFromMasterAccount
  where
  type
    Rs DisassociateFromMasterAccount =
      DisassociateFromMasterAccountResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateFromMasterAccountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateFromMasterAccount

instance Prelude.NFData DisassociateFromMasterAccount

instance
  Prelude.ToHeaders
    DisassociateFromMasterAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisassociateFromMasterAccount where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath DisassociateFromMasterAccount where
  toPath DisassociateFromMasterAccount' {..} =
    Prelude.mconcat
      [ "/detector/",
        Prelude.toBS detectorId,
        "/master/disassociate"
      ]

instance
  Prelude.ToQuery
    DisassociateFromMasterAccount
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateFromMasterAccountResponse' smart constructor.
data DisassociateFromMasterAccountResponse = DisassociateFromMasterAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DisassociateFromMasterAccountResponse
newDisassociateFromMasterAccountResponse pHttpStatus_ =
  DisassociateFromMasterAccountResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateFromMasterAccountResponse_httpStatus :: Lens.Lens' DisassociateFromMasterAccountResponse Prelude.Int
disassociateFromMasterAccountResponse_httpStatus = Lens.lens (\DisassociateFromMasterAccountResponse' {httpStatus} -> httpStatus) (\s@DisassociateFromMasterAccountResponse' {} a -> s {httpStatus = a} :: DisassociateFromMasterAccountResponse)

instance
  Prelude.NFData
    DisassociateFromMasterAccountResponse
