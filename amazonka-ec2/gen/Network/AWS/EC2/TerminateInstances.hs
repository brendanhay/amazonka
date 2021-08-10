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
-- Module      : Network.AWS.EC2.TerminateInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shuts down the specified instances. This operation is idempotent; if you
-- terminate an instance more than once, each call succeeds.
--
-- If you specify multiple instances and the request fails (for example,
-- because of a single incorrect instance ID), none of the instances are
-- terminated.
--
-- Terminated instances remain visible after termination (for approximately
-- one hour).
--
-- By default, Amazon EC2 deletes all EBS volumes that were attached when
-- the instance launched. Volumes attached after instance launch continue
-- running.
--
-- You can stop, start, and terminate EBS-backed instances. You can only
-- terminate instance store-backed instances. What happens to an instance
-- differs if you stop it or terminate it. For example, when you stop an
-- instance, the root device and any other devices attached to the instance
-- persist. When you terminate an instance, any attached EBS volumes with
-- the @DeleteOnTermination@ block device mapping parameter set to @true@
-- are automatically deleted. For more information about the differences
-- between stopping and terminating instances, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html Instance lifecycle>
-- in the /Amazon EC2 User Guide/.
--
-- For more information about troubleshooting, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesShuttingDown.html Troubleshooting terminating your instance>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.TerminateInstances
  ( -- * Creating a Request
    TerminateInstances (..),
    newTerminateInstances,

    -- * Request Lenses
    terminateInstances_dryRun,
    terminateInstances_instanceIds,

    -- * Destructuring the Response
    TerminateInstancesResponse (..),
    newTerminateInstancesResponse,

    -- * Response Lenses
    terminateInstancesResponse_terminatingInstances,
    terminateInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newTerminateInstances' smart constructor.
data TerminateInstances = TerminateInstances'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the instances.
    --
    -- Constraints: Up to 1000 instance IDs. We recommend breaking up this
    -- request into smaller batches.
    instanceIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'terminateInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceIds', 'terminateInstances_instanceIds' - The IDs of the instances.
--
-- Constraints: Up to 1000 instance IDs. We recommend breaking up this
-- request into smaller batches.
newTerminateInstances ::
  TerminateInstances
newTerminateInstances =
  TerminateInstances'
    { dryRun = Prelude.Nothing,
      instanceIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
terminateInstances_dryRun :: Lens.Lens' TerminateInstances (Prelude.Maybe Prelude.Bool)
terminateInstances_dryRun = Lens.lens (\TerminateInstances' {dryRun} -> dryRun) (\s@TerminateInstances' {} a -> s {dryRun = a} :: TerminateInstances)

-- | The IDs of the instances.
--
-- Constraints: Up to 1000 instance IDs. We recommend breaking up this
-- request into smaller batches.
terminateInstances_instanceIds :: Lens.Lens' TerminateInstances [Prelude.Text]
terminateInstances_instanceIds = Lens.lens (\TerminateInstances' {instanceIds} -> instanceIds) (\s@TerminateInstances' {} a -> s {instanceIds = a} :: TerminateInstances) Prelude.. Lens._Coerce

instance Core.AWSRequest TerminateInstances where
  type
    AWSResponse TerminateInstances =
      TerminateInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          TerminateInstancesResponse'
            Prelude.<$> ( x Core..@? "instancesSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TerminateInstances

instance Prelude.NFData TerminateInstances

instance Core.ToHeaders TerminateInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath TerminateInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery TerminateInstances where
  toQuery TerminateInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("TerminateInstances" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQueryList "InstanceId" instanceIds
      ]

-- | /See:/ 'newTerminateInstancesResponse' smart constructor.
data TerminateInstancesResponse = TerminateInstancesResponse'
  { -- | Information about the terminated instances.
    terminatingInstances :: Prelude.Maybe [InstanceStateChange],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'terminatingInstances', 'terminateInstancesResponse_terminatingInstances' - Information about the terminated instances.
--
-- 'httpStatus', 'terminateInstancesResponse_httpStatus' - The response's http status code.
newTerminateInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TerminateInstancesResponse
newTerminateInstancesResponse pHttpStatus_ =
  TerminateInstancesResponse'
    { terminatingInstances =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the terminated instances.
terminateInstancesResponse_terminatingInstances :: Lens.Lens' TerminateInstancesResponse (Prelude.Maybe [InstanceStateChange])
terminateInstancesResponse_terminatingInstances = Lens.lens (\TerminateInstancesResponse' {terminatingInstances} -> terminatingInstances) (\s@TerminateInstancesResponse' {} a -> s {terminatingInstances = a} :: TerminateInstancesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
terminateInstancesResponse_httpStatus :: Lens.Lens' TerminateInstancesResponse Prelude.Int
terminateInstancesResponse_httpStatus = Lens.lens (\TerminateInstancesResponse' {httpStatus} -> httpStatus) (\s@TerminateInstancesResponse' {} a -> s {httpStatus = a} :: TerminateInstancesResponse)

instance Prelude.NFData TerminateInstancesResponse
