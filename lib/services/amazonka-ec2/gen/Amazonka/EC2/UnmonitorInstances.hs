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
-- Module      : Amazonka.EC2.UnmonitorInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables detailed monitoring for a running instance. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html Monitoring your instances and volumes>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.UnmonitorInstances
  ( -- * Creating a Request
    UnmonitorInstances (..),
    newUnmonitorInstances,

    -- * Request Lenses
    unmonitorInstances_dryRun,
    unmonitorInstances_instanceIds,

    -- * Destructuring the Response
    UnmonitorInstancesResponse (..),
    newUnmonitorInstancesResponse,

    -- * Response Lenses
    unmonitorInstancesResponse_instanceMonitorings,
    unmonitorInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUnmonitorInstances' smart constructor.
data UnmonitorInstances = UnmonitorInstances'
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
-- Create a value of 'UnmonitorInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'unmonitorInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceIds', 'unmonitorInstances_instanceIds' - The IDs of the instances.
newUnmonitorInstances ::
  UnmonitorInstances
newUnmonitorInstances =
  UnmonitorInstances'
    { dryRun = Prelude.Nothing,
      instanceIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
unmonitorInstances_dryRun :: Lens.Lens' UnmonitorInstances (Prelude.Maybe Prelude.Bool)
unmonitorInstances_dryRun = Lens.lens (\UnmonitorInstances' {dryRun} -> dryRun) (\s@UnmonitorInstances' {} a -> s {dryRun = a} :: UnmonitorInstances)

-- | The IDs of the instances.
unmonitorInstances_instanceIds :: Lens.Lens' UnmonitorInstances [Prelude.Text]
unmonitorInstances_instanceIds = Lens.lens (\UnmonitorInstances' {instanceIds} -> instanceIds) (\s@UnmonitorInstances' {} a -> s {instanceIds = a} :: UnmonitorInstances) Prelude.. Lens.coerced

instance Core.AWSRequest UnmonitorInstances where
  type
    AWSResponse UnmonitorInstances =
      UnmonitorInstancesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UnmonitorInstancesResponse'
            Prelude.<$> ( x Data..@? "instancesSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UnmonitorInstances where
  hashWithSalt _salt UnmonitorInstances' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceIds

instance Prelude.NFData UnmonitorInstances where
  rnf UnmonitorInstances' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceIds

instance Data.ToHeaders UnmonitorInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UnmonitorInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery UnmonitorInstances where
  toQuery UnmonitorInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UnmonitorInstances" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQueryList "InstanceId" instanceIds
      ]

-- | /See:/ 'newUnmonitorInstancesResponse' smart constructor.
data UnmonitorInstancesResponse = UnmonitorInstancesResponse'
  { -- | The monitoring information.
    instanceMonitorings :: Prelude.Maybe [InstanceMonitoring],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnmonitorInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceMonitorings', 'unmonitorInstancesResponse_instanceMonitorings' - The monitoring information.
--
-- 'httpStatus', 'unmonitorInstancesResponse_httpStatus' - The response's http status code.
newUnmonitorInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UnmonitorInstancesResponse
newUnmonitorInstancesResponse pHttpStatus_ =
  UnmonitorInstancesResponse'
    { instanceMonitorings =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The monitoring information.
unmonitorInstancesResponse_instanceMonitorings :: Lens.Lens' UnmonitorInstancesResponse (Prelude.Maybe [InstanceMonitoring])
unmonitorInstancesResponse_instanceMonitorings = Lens.lens (\UnmonitorInstancesResponse' {instanceMonitorings} -> instanceMonitorings) (\s@UnmonitorInstancesResponse' {} a -> s {instanceMonitorings = a} :: UnmonitorInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
unmonitorInstancesResponse_httpStatus :: Lens.Lens' UnmonitorInstancesResponse Prelude.Int
unmonitorInstancesResponse_httpStatus = Lens.lens (\UnmonitorInstancesResponse' {httpStatus} -> httpStatus) (\s@UnmonitorInstancesResponse' {} a -> s {httpStatus = a} :: UnmonitorInstancesResponse)

instance Prelude.NFData UnmonitorInstancesResponse where
  rnf UnmonitorInstancesResponse' {..} =
    Prelude.rnf instanceMonitorings
      `Prelude.seq` Prelude.rnf httpStatus
