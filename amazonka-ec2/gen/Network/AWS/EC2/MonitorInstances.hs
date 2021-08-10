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
-- Module      : Network.AWS.EC2.MonitorInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables detailed monitoring for a running instance. Otherwise, basic
-- monitoring is enabled. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html Monitoring your instances and volumes>
-- in the /Amazon EC2 User Guide/.
--
-- To disable detailed monitoring, see .
module Network.AWS.EC2.MonitorInstances
  ( -- * Creating a Request
    MonitorInstances (..),
    newMonitorInstances,

    -- * Request Lenses
    monitorInstances_dryRun,
    monitorInstances_instanceIds,

    -- * Destructuring the Response
    MonitorInstancesResponse (..),
    newMonitorInstancesResponse,

    -- * Response Lenses
    monitorInstancesResponse_instanceMonitorings,
    monitorInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newMonitorInstances' smart constructor.
data MonitorInstances = MonitorInstances'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the instances.
    instanceIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitorInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'monitorInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceIds', 'monitorInstances_instanceIds' - The IDs of the instances.
newMonitorInstances ::
  MonitorInstances
newMonitorInstances =
  MonitorInstances'
    { dryRun = Prelude.Nothing,
      instanceIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
monitorInstances_dryRun :: Lens.Lens' MonitorInstances (Prelude.Maybe Prelude.Bool)
monitorInstances_dryRun = Lens.lens (\MonitorInstances' {dryRun} -> dryRun) (\s@MonitorInstances' {} a -> s {dryRun = a} :: MonitorInstances)

-- | The IDs of the instances.
monitorInstances_instanceIds :: Lens.Lens' MonitorInstances [Prelude.Text]
monitorInstances_instanceIds = Lens.lens (\MonitorInstances' {instanceIds} -> instanceIds) (\s@MonitorInstances' {} a -> s {instanceIds = a} :: MonitorInstances) Prelude.. Lens._Coerce

instance Core.AWSRequest MonitorInstances where
  type
    AWSResponse MonitorInstances =
      MonitorInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          MonitorInstancesResponse'
            Prelude.<$> ( x Core..@? "instancesSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable MonitorInstances

instance Prelude.NFData MonitorInstances

instance Core.ToHeaders MonitorInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath MonitorInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery MonitorInstances where
  toQuery MonitorInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("MonitorInstances" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQueryList "InstanceId" instanceIds
      ]

-- | /See:/ 'newMonitorInstancesResponse' smart constructor.
data MonitorInstancesResponse = MonitorInstancesResponse'
  { -- | The monitoring information.
    instanceMonitorings :: Prelude.Maybe [InstanceMonitoring],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitorInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceMonitorings', 'monitorInstancesResponse_instanceMonitorings' - The monitoring information.
--
-- 'httpStatus', 'monitorInstancesResponse_httpStatus' - The response's http status code.
newMonitorInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  MonitorInstancesResponse
newMonitorInstancesResponse pHttpStatus_ =
  MonitorInstancesResponse'
    { instanceMonitorings =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The monitoring information.
monitorInstancesResponse_instanceMonitorings :: Lens.Lens' MonitorInstancesResponse (Prelude.Maybe [InstanceMonitoring])
monitorInstancesResponse_instanceMonitorings = Lens.lens (\MonitorInstancesResponse' {instanceMonitorings} -> instanceMonitorings) (\s@MonitorInstancesResponse' {} a -> s {instanceMonitorings = a} :: MonitorInstancesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
monitorInstancesResponse_httpStatus :: Lens.Lens' MonitorInstancesResponse Prelude.Int
monitorInstancesResponse_httpStatus = Lens.lens (\MonitorInstancesResponse' {httpStatus} -> httpStatus) (\s@MonitorInstancesResponse' {} a -> s {httpStatus = a} :: MonitorInstancesResponse)

instance Prelude.NFData MonitorInstancesResponse
