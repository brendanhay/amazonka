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
-- Module      : Amazonka.EC2.MonitorInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables detailed monitoring for a running instance. Otherwise, basic
-- monitoring is enabled. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html Monitor your instances using CloudWatch>
-- in the /Amazon EC2 User Guide/.
--
-- To disable detailed monitoring, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_UnmonitorInstances.html UnmonitorInstances>.
module Amazonka.EC2.MonitorInstances
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
monitorInstances_instanceIds = Lens.lens (\MonitorInstances' {instanceIds} -> instanceIds) (\s@MonitorInstances' {} a -> s {instanceIds = a} :: MonitorInstances) Prelude.. Lens.coerced

instance Core.AWSRequest MonitorInstances where
  type
    AWSResponse MonitorInstances =
      MonitorInstancesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          MonitorInstancesResponse'
            Prelude.<$> ( x Data..@? "instancesSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable MonitorInstances where
  hashWithSalt _salt MonitorInstances' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceIds

instance Prelude.NFData MonitorInstances where
  rnf MonitorInstances' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceIds

instance Data.ToHeaders MonitorInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath MonitorInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery MonitorInstances where
  toQuery MonitorInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("MonitorInstances" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQueryList "InstanceId" instanceIds
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
monitorInstancesResponse_instanceMonitorings = Lens.lens (\MonitorInstancesResponse' {instanceMonitorings} -> instanceMonitorings) (\s@MonitorInstancesResponse' {} a -> s {instanceMonitorings = a} :: MonitorInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
monitorInstancesResponse_httpStatus :: Lens.Lens' MonitorInstancesResponse Prelude.Int
monitorInstancesResponse_httpStatus = Lens.lens (\MonitorInstancesResponse' {httpStatus} -> httpStatus) (\s@MonitorInstancesResponse' {} a -> s {httpStatus = a} :: MonitorInstancesResponse)

instance Prelude.NFData MonitorInstancesResponse where
  rnf MonitorInstancesResponse' {..} =
    Prelude.rnf instanceMonitorings
      `Prelude.seq` Prelude.rnf httpStatus
