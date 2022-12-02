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
-- Module      : Amazonka.EC2.TerminateInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- If you terminate multiple instances across multiple Availability Zones,
-- and one or more of the specified instances are enabled for termination
-- protection, the request fails with the following results:
--
-- -   The specified instances that are in the same Availability Zone as
--     the protected instance are not terminated.
--
-- -   The specified instances that are in different Availability Zones,
--     where no other specified instances are protected, are successfully
--     terminated.
--
-- For example, say you have the following instances:
--
-- -   Instance A: @us-east-1a@; Not protected
--
-- -   Instance B: @us-east-1a@; Not protected
--
-- -   Instance C: @us-east-1b@; Protected
--
-- -   Instance D: @us-east-1b@; not protected
--
-- If you attempt to terminate all of these instances in the same request,
-- the request reports failure with the following results:
--
-- -   Instance A and Instance B are successfully terminated because none
--     of the specified instances in @us-east-1a@ are enabled for
--     termination protection.
--
-- -   Instance C and Instance D fail to terminate because at least one of
--     the specified instances in @us-east-1b@ (Instance C) is enabled for
--     termination protection.
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
module Amazonka.EC2.TerminateInstances
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
terminateInstances_instanceIds = Lens.lens (\TerminateInstances' {instanceIds} -> instanceIds) (\s@TerminateInstances' {} a -> s {instanceIds = a} :: TerminateInstances) Prelude.. Lens.coerced

instance Core.AWSRequest TerminateInstances where
  type
    AWSResponse TerminateInstances =
      TerminateInstancesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          TerminateInstancesResponse'
            Prelude.<$> ( x Data..@? "instancesSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TerminateInstances where
  hashWithSalt _salt TerminateInstances' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceIds

instance Prelude.NFData TerminateInstances where
  rnf TerminateInstances' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceIds

instance Data.ToHeaders TerminateInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath TerminateInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery TerminateInstances where
  toQuery TerminateInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("TerminateInstances" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQueryList "InstanceId" instanceIds
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
terminateInstancesResponse_terminatingInstances = Lens.lens (\TerminateInstancesResponse' {terminatingInstances} -> terminatingInstances) (\s@TerminateInstancesResponse' {} a -> s {terminatingInstances = a} :: TerminateInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
terminateInstancesResponse_httpStatus :: Lens.Lens' TerminateInstancesResponse Prelude.Int
terminateInstancesResponse_httpStatus = Lens.lens (\TerminateInstancesResponse' {httpStatus} -> httpStatus) (\s@TerminateInstancesResponse' {} a -> s {httpStatus = a} :: TerminateInstancesResponse)

instance Prelude.NFData TerminateInstancesResponse where
  rnf TerminateInstancesResponse' {..} =
    Prelude.rnf terminatingInstances
      `Prelude.seq` Prelude.rnf httpStatus
