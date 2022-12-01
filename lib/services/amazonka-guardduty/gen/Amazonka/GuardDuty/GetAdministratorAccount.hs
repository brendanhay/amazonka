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
-- Module      : Amazonka.GuardDuty.GetAdministratorAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the details for the GuardDuty administrator account associated
-- with the current GuardDuty member account.
module Amazonka.GuardDuty.GetAdministratorAccount
  ( -- * Creating a Request
    GetAdministratorAccount (..),
    newGetAdministratorAccount,

    -- * Request Lenses
    getAdministratorAccount_detectorId,

    -- * Destructuring the Response
    GetAdministratorAccountResponse (..),
    newGetAdministratorAccountResponse,

    -- * Response Lenses
    getAdministratorAccountResponse_httpStatus,
    getAdministratorAccountResponse_administrator,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAdministratorAccount' smart constructor.
data GetAdministratorAccount = GetAdministratorAccount'
  { -- | The unique ID of the detector of the GuardDuty member account.
    detectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAdministratorAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'getAdministratorAccount_detectorId' - The unique ID of the detector of the GuardDuty member account.
newGetAdministratorAccount ::
  -- | 'detectorId'
  Prelude.Text ->
  GetAdministratorAccount
newGetAdministratorAccount pDetectorId_ =
  GetAdministratorAccount' {detectorId = pDetectorId_}

-- | The unique ID of the detector of the GuardDuty member account.
getAdministratorAccount_detectorId :: Lens.Lens' GetAdministratorAccount Prelude.Text
getAdministratorAccount_detectorId = Lens.lens (\GetAdministratorAccount' {detectorId} -> detectorId) (\s@GetAdministratorAccount' {} a -> s {detectorId = a} :: GetAdministratorAccount)

instance Core.AWSRequest GetAdministratorAccount where
  type
    AWSResponse GetAdministratorAccount =
      GetAdministratorAccountResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAdministratorAccountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "administrator")
      )

instance Prelude.Hashable GetAdministratorAccount where
  hashWithSalt _salt GetAdministratorAccount' {..} =
    _salt `Prelude.hashWithSalt` detectorId

instance Prelude.NFData GetAdministratorAccount where
  rnf GetAdministratorAccount' {..} =
    Prelude.rnf detectorId

instance Core.ToHeaders GetAdministratorAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetAdministratorAccount where
  toPath GetAdministratorAccount' {..} =
    Prelude.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/administrator"
      ]

instance Core.ToQuery GetAdministratorAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAdministratorAccountResponse' smart constructor.
data GetAdministratorAccountResponse = GetAdministratorAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The administrator account details.
    administrator :: Administrator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAdministratorAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getAdministratorAccountResponse_httpStatus' - The response's http status code.
--
-- 'administrator', 'getAdministratorAccountResponse_administrator' - The administrator account details.
newGetAdministratorAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'administrator'
  Administrator ->
  GetAdministratorAccountResponse
newGetAdministratorAccountResponse
  pHttpStatus_
  pAdministrator_ =
    GetAdministratorAccountResponse'
      { httpStatus =
          pHttpStatus_,
        administrator = pAdministrator_
      }

-- | The response's http status code.
getAdministratorAccountResponse_httpStatus :: Lens.Lens' GetAdministratorAccountResponse Prelude.Int
getAdministratorAccountResponse_httpStatus = Lens.lens (\GetAdministratorAccountResponse' {httpStatus} -> httpStatus) (\s@GetAdministratorAccountResponse' {} a -> s {httpStatus = a} :: GetAdministratorAccountResponse)

-- | The administrator account details.
getAdministratorAccountResponse_administrator :: Lens.Lens' GetAdministratorAccountResponse Administrator
getAdministratorAccountResponse_administrator = Lens.lens (\GetAdministratorAccountResponse' {administrator} -> administrator) (\s@GetAdministratorAccountResponse' {} a -> s {administrator = a} :: GetAdministratorAccountResponse)

instance
  Prelude.NFData
    GetAdministratorAccountResponse
  where
  rnf GetAdministratorAccountResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf administrator
