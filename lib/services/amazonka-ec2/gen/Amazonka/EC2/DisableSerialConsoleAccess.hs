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
-- Module      : Amazonka.EC2.DisableSerialConsoleAccess
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables access to the EC2 serial console of all instances for your
-- account. By default, access to the EC2 serial console is disabled for
-- your account. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/configure-access-to-serial-console.html#serial-console-account-access Manage account access to the EC2 serial console>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.DisableSerialConsoleAccess
  ( -- * Creating a Request
    DisableSerialConsoleAccess (..),
    newDisableSerialConsoleAccess,

    -- * Request Lenses
    disableSerialConsoleAccess_dryRun,

    -- * Destructuring the Response
    DisableSerialConsoleAccessResponse (..),
    newDisableSerialConsoleAccessResponse,

    -- * Response Lenses
    disableSerialConsoleAccessResponse_serialConsoleAccessEnabled,
    disableSerialConsoleAccessResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableSerialConsoleAccess' smart constructor.
data DisableSerialConsoleAccess = DisableSerialConsoleAccess'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableSerialConsoleAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'disableSerialConsoleAccess_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newDisableSerialConsoleAccess ::
  DisableSerialConsoleAccess
newDisableSerialConsoleAccess =
  DisableSerialConsoleAccess'
    { dryRun =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
disableSerialConsoleAccess_dryRun :: Lens.Lens' DisableSerialConsoleAccess (Prelude.Maybe Prelude.Bool)
disableSerialConsoleAccess_dryRun = Lens.lens (\DisableSerialConsoleAccess' {dryRun} -> dryRun) (\s@DisableSerialConsoleAccess' {} a -> s {dryRun = a} :: DisableSerialConsoleAccess)

instance Core.AWSRequest DisableSerialConsoleAccess where
  type
    AWSResponse DisableSerialConsoleAccess =
      DisableSerialConsoleAccessResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DisableSerialConsoleAccessResponse'
            Prelude.<$> (x Core..@? "serialConsoleAccessEnabled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableSerialConsoleAccess where
  hashWithSalt _salt DisableSerialConsoleAccess' {..} =
    _salt `Prelude.hashWithSalt` dryRun

instance Prelude.NFData DisableSerialConsoleAccess where
  rnf DisableSerialConsoleAccess' {..} =
    Prelude.rnf dryRun

instance Core.ToHeaders DisableSerialConsoleAccess where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DisableSerialConsoleAccess where
  toPath = Prelude.const "/"

instance Core.ToQuery DisableSerialConsoleAccess where
  toQuery DisableSerialConsoleAccess' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DisableSerialConsoleAccess" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun
      ]

-- | /See:/ 'newDisableSerialConsoleAccessResponse' smart constructor.
data DisableSerialConsoleAccessResponse = DisableSerialConsoleAccessResponse'
  { -- | If @true@, access to the EC2 serial console of all instances is enabled
    -- for your account. If @false@, access to the EC2 serial console of all
    -- instances is disabled for your account.
    serialConsoleAccessEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableSerialConsoleAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serialConsoleAccessEnabled', 'disableSerialConsoleAccessResponse_serialConsoleAccessEnabled' - If @true@, access to the EC2 serial console of all instances is enabled
-- for your account. If @false@, access to the EC2 serial console of all
-- instances is disabled for your account.
--
-- 'httpStatus', 'disableSerialConsoleAccessResponse_httpStatus' - The response's http status code.
newDisableSerialConsoleAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableSerialConsoleAccessResponse
newDisableSerialConsoleAccessResponse pHttpStatus_ =
  DisableSerialConsoleAccessResponse'
    { serialConsoleAccessEnabled =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If @true@, access to the EC2 serial console of all instances is enabled
-- for your account. If @false@, access to the EC2 serial console of all
-- instances is disabled for your account.
disableSerialConsoleAccessResponse_serialConsoleAccessEnabled :: Lens.Lens' DisableSerialConsoleAccessResponse (Prelude.Maybe Prelude.Bool)
disableSerialConsoleAccessResponse_serialConsoleAccessEnabled = Lens.lens (\DisableSerialConsoleAccessResponse' {serialConsoleAccessEnabled} -> serialConsoleAccessEnabled) (\s@DisableSerialConsoleAccessResponse' {} a -> s {serialConsoleAccessEnabled = a} :: DisableSerialConsoleAccessResponse)

-- | The response's http status code.
disableSerialConsoleAccessResponse_httpStatus :: Lens.Lens' DisableSerialConsoleAccessResponse Prelude.Int
disableSerialConsoleAccessResponse_httpStatus = Lens.lens (\DisableSerialConsoleAccessResponse' {httpStatus} -> httpStatus) (\s@DisableSerialConsoleAccessResponse' {} a -> s {httpStatus = a} :: DisableSerialConsoleAccessResponse)

instance
  Prelude.NFData
    DisableSerialConsoleAccessResponse
  where
  rnf DisableSerialConsoleAccessResponse' {..} =
    Prelude.rnf serialConsoleAccessEnabled
      `Prelude.seq` Prelude.rnf httpStatus
