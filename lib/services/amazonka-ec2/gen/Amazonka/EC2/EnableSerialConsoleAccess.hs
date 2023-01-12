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
-- Module      : Amazonka.EC2.EnableSerialConsoleAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables access to the EC2 serial console of all instances for your
-- account. By default, access to the EC2 serial console is disabled for
-- your account. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/configure-access-to-serial-console.html#serial-console-account-access Manage account access to the EC2 serial console>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.EnableSerialConsoleAccess
  ( -- * Creating a Request
    EnableSerialConsoleAccess (..),
    newEnableSerialConsoleAccess,

    -- * Request Lenses
    enableSerialConsoleAccess_dryRun,

    -- * Destructuring the Response
    EnableSerialConsoleAccessResponse (..),
    newEnableSerialConsoleAccessResponse,

    -- * Response Lenses
    enableSerialConsoleAccessResponse_serialConsoleAccessEnabled,
    enableSerialConsoleAccessResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableSerialConsoleAccess' smart constructor.
data EnableSerialConsoleAccess = EnableSerialConsoleAccess'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableSerialConsoleAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'enableSerialConsoleAccess_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newEnableSerialConsoleAccess ::
  EnableSerialConsoleAccess
newEnableSerialConsoleAccess =
  EnableSerialConsoleAccess'
    { dryRun =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
enableSerialConsoleAccess_dryRun :: Lens.Lens' EnableSerialConsoleAccess (Prelude.Maybe Prelude.Bool)
enableSerialConsoleAccess_dryRun = Lens.lens (\EnableSerialConsoleAccess' {dryRun} -> dryRun) (\s@EnableSerialConsoleAccess' {} a -> s {dryRun = a} :: EnableSerialConsoleAccess)

instance Core.AWSRequest EnableSerialConsoleAccess where
  type
    AWSResponse EnableSerialConsoleAccess =
      EnableSerialConsoleAccessResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          EnableSerialConsoleAccessResponse'
            Prelude.<$> (x Data..@? "serialConsoleAccessEnabled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableSerialConsoleAccess where
  hashWithSalt _salt EnableSerialConsoleAccess' {..} =
    _salt `Prelude.hashWithSalt` dryRun

instance Prelude.NFData EnableSerialConsoleAccess where
  rnf EnableSerialConsoleAccess' {..} =
    Prelude.rnf dryRun

instance Data.ToHeaders EnableSerialConsoleAccess where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath EnableSerialConsoleAccess where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableSerialConsoleAccess where
  toQuery EnableSerialConsoleAccess' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("EnableSerialConsoleAccess" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun
      ]

-- | /See:/ 'newEnableSerialConsoleAccessResponse' smart constructor.
data EnableSerialConsoleAccessResponse = EnableSerialConsoleAccessResponse'
  { -- | If @true@, access to the EC2 serial console of all instances is enabled
    -- for your account. If @false@, access to the EC2 serial console of all
    -- instances is disabled for your account.
    serialConsoleAccessEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableSerialConsoleAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serialConsoleAccessEnabled', 'enableSerialConsoleAccessResponse_serialConsoleAccessEnabled' - If @true@, access to the EC2 serial console of all instances is enabled
-- for your account. If @false@, access to the EC2 serial console of all
-- instances is disabled for your account.
--
-- 'httpStatus', 'enableSerialConsoleAccessResponse_httpStatus' - The response's http status code.
newEnableSerialConsoleAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableSerialConsoleAccessResponse
newEnableSerialConsoleAccessResponse pHttpStatus_ =
  EnableSerialConsoleAccessResponse'
    { serialConsoleAccessEnabled =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If @true@, access to the EC2 serial console of all instances is enabled
-- for your account. If @false@, access to the EC2 serial console of all
-- instances is disabled for your account.
enableSerialConsoleAccessResponse_serialConsoleAccessEnabled :: Lens.Lens' EnableSerialConsoleAccessResponse (Prelude.Maybe Prelude.Bool)
enableSerialConsoleAccessResponse_serialConsoleAccessEnabled = Lens.lens (\EnableSerialConsoleAccessResponse' {serialConsoleAccessEnabled} -> serialConsoleAccessEnabled) (\s@EnableSerialConsoleAccessResponse' {} a -> s {serialConsoleAccessEnabled = a} :: EnableSerialConsoleAccessResponse)

-- | The response's http status code.
enableSerialConsoleAccessResponse_httpStatus :: Lens.Lens' EnableSerialConsoleAccessResponse Prelude.Int
enableSerialConsoleAccessResponse_httpStatus = Lens.lens (\EnableSerialConsoleAccessResponse' {httpStatus} -> httpStatus) (\s@EnableSerialConsoleAccessResponse' {} a -> s {httpStatus = a} :: EnableSerialConsoleAccessResponse)

instance
  Prelude.NFData
    EnableSerialConsoleAccessResponse
  where
  rnf EnableSerialConsoleAccessResponse' {..} =
    Prelude.rnf serialConsoleAccessEnabled
      `Prelude.seq` Prelude.rnf httpStatus
