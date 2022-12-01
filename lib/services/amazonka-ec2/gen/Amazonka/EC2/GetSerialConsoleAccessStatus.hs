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
-- Module      : Amazonka.EC2.GetSerialConsoleAccessStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the access status of your account to the EC2 serial console of
-- all instances. By default, access to the EC2 serial console is disabled
-- for your account. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/configure-access-to-serial-console.html#serial-console-account-access Manage account access to the EC2 serial console>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.GetSerialConsoleAccessStatus
  ( -- * Creating a Request
    GetSerialConsoleAccessStatus (..),
    newGetSerialConsoleAccessStatus,

    -- * Request Lenses
    getSerialConsoleAccessStatus_dryRun,

    -- * Destructuring the Response
    GetSerialConsoleAccessStatusResponse (..),
    newGetSerialConsoleAccessStatusResponse,

    -- * Response Lenses
    getSerialConsoleAccessStatusResponse_serialConsoleAccessEnabled,
    getSerialConsoleAccessStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSerialConsoleAccessStatus' smart constructor.
data GetSerialConsoleAccessStatus = GetSerialConsoleAccessStatus'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSerialConsoleAccessStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getSerialConsoleAccessStatus_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newGetSerialConsoleAccessStatus ::
  GetSerialConsoleAccessStatus
newGetSerialConsoleAccessStatus =
  GetSerialConsoleAccessStatus'
    { dryRun =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getSerialConsoleAccessStatus_dryRun :: Lens.Lens' GetSerialConsoleAccessStatus (Prelude.Maybe Prelude.Bool)
getSerialConsoleAccessStatus_dryRun = Lens.lens (\GetSerialConsoleAccessStatus' {dryRun} -> dryRun) (\s@GetSerialConsoleAccessStatus' {} a -> s {dryRun = a} :: GetSerialConsoleAccessStatus)

instance Core.AWSRequest GetSerialConsoleAccessStatus where
  type
    AWSResponse GetSerialConsoleAccessStatus =
      GetSerialConsoleAccessStatusResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetSerialConsoleAccessStatusResponse'
            Prelude.<$> (x Core..@? "serialConsoleAccessEnabled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetSerialConsoleAccessStatus
  where
  hashWithSalt _salt GetSerialConsoleAccessStatus' {..} =
    _salt `Prelude.hashWithSalt` dryRun

instance Prelude.NFData GetSerialConsoleAccessStatus where
  rnf GetSerialConsoleAccessStatus' {..} =
    Prelude.rnf dryRun

instance Core.ToHeaders GetSerialConsoleAccessStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetSerialConsoleAccessStatus where
  toPath = Prelude.const "/"

instance Core.ToQuery GetSerialConsoleAccessStatus where
  toQuery GetSerialConsoleAccessStatus' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "GetSerialConsoleAccessStatus" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun
      ]

-- | /See:/ 'newGetSerialConsoleAccessStatusResponse' smart constructor.
data GetSerialConsoleAccessStatusResponse = GetSerialConsoleAccessStatusResponse'
  { -- | If @true@, access to the EC2 serial console of all instances is enabled
    -- for your account. If @false@, access to the EC2 serial console of all
    -- instances is disabled for your account.
    serialConsoleAccessEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSerialConsoleAccessStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serialConsoleAccessEnabled', 'getSerialConsoleAccessStatusResponse_serialConsoleAccessEnabled' - If @true@, access to the EC2 serial console of all instances is enabled
-- for your account. If @false@, access to the EC2 serial console of all
-- instances is disabled for your account.
--
-- 'httpStatus', 'getSerialConsoleAccessStatusResponse_httpStatus' - The response's http status code.
newGetSerialConsoleAccessStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSerialConsoleAccessStatusResponse
newGetSerialConsoleAccessStatusResponse pHttpStatus_ =
  GetSerialConsoleAccessStatusResponse'
    { serialConsoleAccessEnabled =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If @true@, access to the EC2 serial console of all instances is enabled
-- for your account. If @false@, access to the EC2 serial console of all
-- instances is disabled for your account.
getSerialConsoleAccessStatusResponse_serialConsoleAccessEnabled :: Lens.Lens' GetSerialConsoleAccessStatusResponse (Prelude.Maybe Prelude.Bool)
getSerialConsoleAccessStatusResponse_serialConsoleAccessEnabled = Lens.lens (\GetSerialConsoleAccessStatusResponse' {serialConsoleAccessEnabled} -> serialConsoleAccessEnabled) (\s@GetSerialConsoleAccessStatusResponse' {} a -> s {serialConsoleAccessEnabled = a} :: GetSerialConsoleAccessStatusResponse)

-- | The response's http status code.
getSerialConsoleAccessStatusResponse_httpStatus :: Lens.Lens' GetSerialConsoleAccessStatusResponse Prelude.Int
getSerialConsoleAccessStatusResponse_httpStatus = Lens.lens (\GetSerialConsoleAccessStatusResponse' {httpStatus} -> httpStatus) (\s@GetSerialConsoleAccessStatusResponse' {} a -> s {httpStatus = a} :: GetSerialConsoleAccessStatusResponse)

instance
  Prelude.NFData
    GetSerialConsoleAccessStatusResponse
  where
  rnf GetSerialConsoleAccessStatusResponse' {..} =
    Prelude.rnf serialConsoleAccessEnabled
      `Prelude.seq` Prelude.rnf httpStatus
