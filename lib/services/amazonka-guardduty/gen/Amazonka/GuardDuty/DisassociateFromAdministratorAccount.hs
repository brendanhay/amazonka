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
-- Module      : Amazonka.GuardDuty.DisassociateFromAdministratorAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the current GuardDuty member account from its
-- administrator account.
module Amazonka.GuardDuty.DisassociateFromAdministratorAccount
  ( -- * Creating a Request
    DisassociateFromAdministratorAccount (..),
    newDisassociateFromAdministratorAccount,

    -- * Request Lenses
    disassociateFromAdministratorAccount_detectorId,

    -- * Destructuring the Response
    DisassociateFromAdministratorAccountResponse (..),
    newDisassociateFromAdministratorAccountResponse,

    -- * Response Lenses
    disassociateFromAdministratorAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateFromAdministratorAccount' smart constructor.
data DisassociateFromAdministratorAccount = DisassociateFromAdministratorAccount'
  { -- | The unique ID of the detector of the GuardDuty member account.
    detectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateFromAdministratorAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'disassociateFromAdministratorAccount_detectorId' - The unique ID of the detector of the GuardDuty member account.
newDisassociateFromAdministratorAccount ::
  -- | 'detectorId'
  Prelude.Text ->
  DisassociateFromAdministratorAccount
newDisassociateFromAdministratorAccount pDetectorId_ =
  DisassociateFromAdministratorAccount'
    { detectorId =
        pDetectorId_
    }

-- | The unique ID of the detector of the GuardDuty member account.
disassociateFromAdministratorAccount_detectorId :: Lens.Lens' DisassociateFromAdministratorAccount Prelude.Text
disassociateFromAdministratorAccount_detectorId = Lens.lens (\DisassociateFromAdministratorAccount' {detectorId} -> detectorId) (\s@DisassociateFromAdministratorAccount' {} a -> s {detectorId = a} :: DisassociateFromAdministratorAccount)

instance
  Core.AWSRequest
    DisassociateFromAdministratorAccount
  where
  type
    AWSResponse DisassociateFromAdministratorAccount =
      DisassociateFromAdministratorAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateFromAdministratorAccountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateFromAdministratorAccount
  where
  hashWithSalt
    _salt
    DisassociateFromAdministratorAccount' {..} =
      _salt `Prelude.hashWithSalt` detectorId

instance
  Prelude.NFData
    DisassociateFromAdministratorAccount
  where
  rnf DisassociateFromAdministratorAccount' {..} =
    Prelude.rnf detectorId

instance
  Data.ToHeaders
    DisassociateFromAdministratorAccount
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DisassociateFromAdministratorAccount
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance
  Data.ToPath
    DisassociateFromAdministratorAccount
  where
  toPath DisassociateFromAdministratorAccount' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/administrator/disassociate"
      ]

instance
  Data.ToQuery
    DisassociateFromAdministratorAccount
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateFromAdministratorAccountResponse' smart constructor.
data DisassociateFromAdministratorAccountResponse = DisassociateFromAdministratorAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateFromAdministratorAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateFromAdministratorAccountResponse_httpStatus' - The response's http status code.
newDisassociateFromAdministratorAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateFromAdministratorAccountResponse
newDisassociateFromAdministratorAccountResponse
  pHttpStatus_ =
    DisassociateFromAdministratorAccountResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateFromAdministratorAccountResponse_httpStatus :: Lens.Lens' DisassociateFromAdministratorAccountResponse Prelude.Int
disassociateFromAdministratorAccountResponse_httpStatus = Lens.lens (\DisassociateFromAdministratorAccountResponse' {httpStatus} -> httpStatus) (\s@DisassociateFromAdministratorAccountResponse' {} a -> s {httpStatus = a} :: DisassociateFromAdministratorAccountResponse)

instance
  Prelude.NFData
    DisassociateFromAdministratorAccountResponse
  where
  rnf DisassociateFromAdministratorAccountResponse' {..} =
    Prelude.rnf httpStatus
