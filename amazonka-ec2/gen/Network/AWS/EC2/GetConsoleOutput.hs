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
-- Module      : Network.AWS.EC2.GetConsoleOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the console output for the specified instance. For Linux instances,
-- the instance console output displays the exact console output that would
-- normally be displayed on a physical monitor attached to a computer. For
-- Windows instances, the instance console output includes the last three
-- system event log errors.
--
-- By default, the console output returns buffered information that was
-- posted shortly after an instance transition state (start, stop, reboot,
-- or terminate). This information is available for at least one hour after
-- the most recent post. Only the most recent 64 KB of console output is
-- available.
--
-- You can optionally retrieve the latest serial console output at any time
-- during the instance lifecycle. This option is supported on instance
-- types that use the Nitro hypervisor.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-console.html#instance-console-console-output Instance console output>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.GetConsoleOutput
  ( -- * Creating a Request
    GetConsoleOutput (..),
    newGetConsoleOutput,

    -- * Request Lenses
    getConsoleOutput_dryRun,
    getConsoleOutput_latest,
    getConsoleOutput_instanceId,

    -- * Destructuring the Response
    GetConsoleOutputResponse (..),
    newGetConsoleOutputResponse,

    -- * Response Lenses
    getConsoleOutputResponse_instanceId,
    getConsoleOutputResponse_output,
    getConsoleOutputResponse_timestamp,
    getConsoleOutputResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetConsoleOutput' smart constructor.
data GetConsoleOutput = GetConsoleOutput'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | When enabled, retrieves the latest console output for the instance.
    --
    -- Default: disabled (@false@)
    latest :: Core.Maybe Core.Bool,
    -- | The ID of the instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConsoleOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'getConsoleOutput_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'latest', 'getConsoleOutput_latest' - When enabled, retrieves the latest console output for the instance.
--
-- Default: disabled (@false@)
--
-- 'instanceId', 'getConsoleOutput_instanceId' - The ID of the instance.
newGetConsoleOutput ::
  -- | 'instanceId'
  Core.Text ->
  GetConsoleOutput
newGetConsoleOutput pInstanceId_ =
  GetConsoleOutput'
    { dryRun = Core.Nothing,
      latest = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getConsoleOutput_dryRun :: Lens.Lens' GetConsoleOutput (Core.Maybe Core.Bool)
getConsoleOutput_dryRun = Lens.lens (\GetConsoleOutput' {dryRun} -> dryRun) (\s@GetConsoleOutput' {} a -> s {dryRun = a} :: GetConsoleOutput)

-- | When enabled, retrieves the latest console output for the instance.
--
-- Default: disabled (@false@)
getConsoleOutput_latest :: Lens.Lens' GetConsoleOutput (Core.Maybe Core.Bool)
getConsoleOutput_latest = Lens.lens (\GetConsoleOutput' {latest} -> latest) (\s@GetConsoleOutput' {} a -> s {latest = a} :: GetConsoleOutput)

-- | The ID of the instance.
getConsoleOutput_instanceId :: Lens.Lens' GetConsoleOutput Core.Text
getConsoleOutput_instanceId = Lens.lens (\GetConsoleOutput' {instanceId} -> instanceId) (\s@GetConsoleOutput' {} a -> s {instanceId = a} :: GetConsoleOutput)

instance Core.AWSRequest GetConsoleOutput where
  type
    AWSResponse GetConsoleOutput =
      GetConsoleOutputResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetConsoleOutputResponse'
            Core.<$> (x Core..@? "instanceId")
            Core.<*> (x Core..@? "output")
            Core.<*> (x Core..@? "timestamp")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetConsoleOutput

instance Core.NFData GetConsoleOutput

instance Core.ToHeaders GetConsoleOutput where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetConsoleOutput where
  toPath = Core.const "/"

instance Core.ToQuery GetConsoleOutput where
  toQuery GetConsoleOutput' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("GetConsoleOutput" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "Latest" Core.=: latest,
        "InstanceId" Core.=: instanceId
      ]

-- | /See:/ 'newGetConsoleOutputResponse' smart constructor.
data GetConsoleOutputResponse = GetConsoleOutputResponse'
  { -- | The ID of the instance.
    instanceId :: Core.Maybe Core.Text,
    -- | The console output, base64-encoded. If you are using a command line
    -- tool, the tool decodes the output for you.
    output :: Core.Maybe Core.Text,
    -- | The time at which the output was last updated.
    timestamp :: Core.Maybe Core.ISO8601,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConsoleOutputResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'getConsoleOutputResponse_instanceId' - The ID of the instance.
--
-- 'output', 'getConsoleOutputResponse_output' - The console output, base64-encoded. If you are using a command line
-- tool, the tool decodes the output for you.
--
-- 'timestamp', 'getConsoleOutputResponse_timestamp' - The time at which the output was last updated.
--
-- 'httpStatus', 'getConsoleOutputResponse_httpStatus' - The response's http status code.
newGetConsoleOutputResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetConsoleOutputResponse
newGetConsoleOutputResponse pHttpStatus_ =
  GetConsoleOutputResponse'
    { instanceId =
        Core.Nothing,
      output = Core.Nothing,
      timestamp = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the instance.
getConsoleOutputResponse_instanceId :: Lens.Lens' GetConsoleOutputResponse (Core.Maybe Core.Text)
getConsoleOutputResponse_instanceId = Lens.lens (\GetConsoleOutputResponse' {instanceId} -> instanceId) (\s@GetConsoleOutputResponse' {} a -> s {instanceId = a} :: GetConsoleOutputResponse)

-- | The console output, base64-encoded. If you are using a command line
-- tool, the tool decodes the output for you.
getConsoleOutputResponse_output :: Lens.Lens' GetConsoleOutputResponse (Core.Maybe Core.Text)
getConsoleOutputResponse_output = Lens.lens (\GetConsoleOutputResponse' {output} -> output) (\s@GetConsoleOutputResponse' {} a -> s {output = a} :: GetConsoleOutputResponse)

-- | The time at which the output was last updated.
getConsoleOutputResponse_timestamp :: Lens.Lens' GetConsoleOutputResponse (Core.Maybe Core.UTCTime)
getConsoleOutputResponse_timestamp = Lens.lens (\GetConsoleOutputResponse' {timestamp} -> timestamp) (\s@GetConsoleOutputResponse' {} a -> s {timestamp = a} :: GetConsoleOutputResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
getConsoleOutputResponse_httpStatus :: Lens.Lens' GetConsoleOutputResponse Core.Int
getConsoleOutputResponse_httpStatus = Lens.lens (\GetConsoleOutputResponse' {httpStatus} -> httpStatus) (\s@GetConsoleOutputResponse' {} a -> s {httpStatus = a} :: GetConsoleOutputResponse)

instance Core.NFData GetConsoleOutputResponse
