{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.UnmonitorInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables detailed monitoring for a running instance. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html Monitoring your instances and volumes>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.UnmonitorInstances
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
unmonitorInstances_instanceIds = Lens.lens (\UnmonitorInstances' {instanceIds} -> instanceIds) (\s@UnmonitorInstances' {} a -> s {instanceIds = a} :: UnmonitorInstances) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest UnmonitorInstances where
  type
    Rs UnmonitorInstances =
      UnmonitorInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          UnmonitorInstancesResponse'
            Prelude.<$> ( x Prelude..@? "instancesSet"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UnmonitorInstances

instance Prelude.NFData UnmonitorInstances

instance Prelude.ToHeaders UnmonitorInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UnmonitorInstances where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UnmonitorInstances where
  toQuery UnmonitorInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UnmonitorInstances" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        Prelude.toQueryList "InstanceId" instanceIds
      ]

-- | /See:/ 'newUnmonitorInstancesResponse' smart constructor.
data UnmonitorInstancesResponse = UnmonitorInstancesResponse'
  { -- | The monitoring information.
    instanceMonitorings :: Prelude.Maybe [InstanceMonitoring],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
unmonitorInstancesResponse_instanceMonitorings = Lens.lens (\UnmonitorInstancesResponse' {instanceMonitorings} -> instanceMonitorings) (\s@UnmonitorInstancesResponse' {} a -> s {instanceMonitorings = a} :: UnmonitorInstancesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
unmonitorInstancesResponse_httpStatus :: Lens.Lens' UnmonitorInstancesResponse Prelude.Int
unmonitorInstancesResponse_httpStatus = Lens.lens (\UnmonitorInstancesResponse' {httpStatus} -> httpStatus) (\s@UnmonitorInstancesResponse' {} a -> s {httpStatus = a} :: UnmonitorInstancesResponse)

instance Prelude.NFData UnmonitorInstancesResponse
