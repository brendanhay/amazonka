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
-- Module      : Amazonka.EC2.SendDiagnosticInterrupt
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a diagnostic interrupt to the specified Amazon EC2 instance to
-- trigger a /kernel panic/ (on Linux instances), or a /blue screen/\//stop
-- error/ (on Windows instances). For instances based on Intel and AMD
-- processors, the interrupt is received as a /non-maskable interrupt/
-- (NMI).
--
-- In general, the operating system crashes and reboots when a kernel panic
-- or stop error is triggered. The operating system can also be configured
-- to perform diagnostic tasks, such as generating a memory dump file,
-- loading a secondary kernel, or obtaining a call trace.
--
-- Before sending a diagnostic interrupt to your instance, ensure that its
-- operating system is configured to perform the required diagnostic tasks.
--
-- For more information about configuring your operating system to generate
-- a crash dump when a kernel panic or stop error occurs, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/diagnostic-interrupt.html Send a diagnostic interrupt (for advanced users)>
-- (Linux instances) or
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/diagnostic-interrupt.html Send a diagnostic interrupt (for advanced users)>
-- (Windows instances).
module Amazonka.EC2.SendDiagnosticInterrupt
  ( -- * Creating a Request
    SendDiagnosticInterrupt (..),
    newSendDiagnosticInterrupt,

    -- * Request Lenses
    sendDiagnosticInterrupt_dryRun,
    sendDiagnosticInterrupt_instanceId,

    -- * Destructuring the Response
    SendDiagnosticInterruptResponse (..),
    newSendDiagnosticInterruptResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendDiagnosticInterrupt' smart constructor.
data SendDiagnosticInterrupt = SendDiagnosticInterrupt'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendDiagnosticInterrupt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'sendDiagnosticInterrupt_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceId', 'sendDiagnosticInterrupt_instanceId' - The ID of the instance.
newSendDiagnosticInterrupt ::
  -- | 'instanceId'
  Prelude.Text ->
  SendDiagnosticInterrupt
newSendDiagnosticInterrupt pInstanceId_ =
  SendDiagnosticInterrupt'
    { dryRun = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
sendDiagnosticInterrupt_dryRun :: Lens.Lens' SendDiagnosticInterrupt (Prelude.Maybe Prelude.Bool)
sendDiagnosticInterrupt_dryRun = Lens.lens (\SendDiagnosticInterrupt' {dryRun} -> dryRun) (\s@SendDiagnosticInterrupt' {} a -> s {dryRun = a} :: SendDiagnosticInterrupt)

-- | The ID of the instance.
sendDiagnosticInterrupt_instanceId :: Lens.Lens' SendDiagnosticInterrupt Prelude.Text
sendDiagnosticInterrupt_instanceId = Lens.lens (\SendDiagnosticInterrupt' {instanceId} -> instanceId) (\s@SendDiagnosticInterrupt' {} a -> s {instanceId = a} :: SendDiagnosticInterrupt)

instance Core.AWSRequest SendDiagnosticInterrupt where
  type
    AWSResponse SendDiagnosticInterrupt =
      SendDiagnosticInterruptResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      SendDiagnosticInterruptResponse'

instance Prelude.Hashable SendDiagnosticInterrupt where
  hashWithSalt _salt SendDiagnosticInterrupt' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData SendDiagnosticInterrupt where
  rnf SendDiagnosticInterrupt' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders SendDiagnosticInterrupt where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SendDiagnosticInterrupt where
  toPath = Prelude.const "/"

instance Data.ToQuery SendDiagnosticInterrupt where
  toQuery SendDiagnosticInterrupt' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SendDiagnosticInterrupt" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "InstanceId" Data.=: instanceId
      ]

-- | /See:/ 'newSendDiagnosticInterruptResponse' smart constructor.
data SendDiagnosticInterruptResponse = SendDiagnosticInterruptResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendDiagnosticInterruptResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSendDiagnosticInterruptResponse ::
  SendDiagnosticInterruptResponse
newSendDiagnosticInterruptResponse =
  SendDiagnosticInterruptResponse'

instance
  Prelude.NFData
    SendDiagnosticInterruptResponse
  where
  rnf _ = ()
