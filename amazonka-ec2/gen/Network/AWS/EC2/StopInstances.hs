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
-- Module      : Network.AWS.EC2.StopInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an Amazon EBS-backed instance.
--
-- You can use the Stop action to hibernate an instance if the instance is
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#enabling-hibernation enabled for hibernation>
-- and it meets the
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html#hibernating-prerequisites hibernation prerequisites>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance>
-- in the /Amazon EC2 User Guide/.
--
-- We don\'t charge usage for a stopped instance, or data transfer fees;
-- however, your root partition Amazon EBS volume remains and continues to
-- persist your data, and you are charged for Amazon EBS volume usage.
-- Every time you start your Windows instance, Amazon EC2 charges you for a
-- full instance hour. If you stop and restart your Windows instance, a new
-- instance hour begins and Amazon EC2 charges you for another full
-- instance hour even if you are still within the same 60-minute period
-- when it was stopped. Every time you start your Linux instance, Amazon
-- EC2 charges a one-minute minimum for instance usage, and thereafter
-- charges per second for instance usage.
--
-- You can\'t stop or hibernate instance store-backed instances. You can\'t
-- use the Stop action to hibernate Spot Instances, but you can specify
-- that Amazon EC2 should hibernate Spot Instances when they are
-- interrupted. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-interruptions.html#hibernate-spot-instances Hibernating interrupted Spot Instances>
-- in the /Amazon EC2 User Guide/.
--
-- When you stop or hibernate an instance, we shut it down. You can restart
-- your instance at any time. Before stopping or hibernating an instance,
-- make sure it is in a state from which it can be restarted. Stopping an
-- instance does not preserve data stored in RAM, but hibernating an
-- instance does preserve data stored in RAM. If an instance cannot
-- hibernate successfully, a normal shutdown occurs.
--
-- Stopping and hibernating an instance is different to rebooting or
-- terminating it. For example, when you stop or hibernate an instance, the
-- root device and any other devices attached to the instance persist. When
-- you terminate an instance, the root device and any other devices
-- attached during the instance launch are automatically deleted. For more
-- information about the differences between rebooting, stopping,
-- hibernating, and terminating instances, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html Instance lifecycle>
-- in the /Amazon EC2 User Guide/.
--
-- When you stop an instance, we attempt to shut it down forcibly after a
-- short while. If your instance appears stuck in the stopping state after
-- a period of time, there may be an issue with the underlying host
-- computer. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstancesStopping.html Troubleshooting stopping your instance>
-- in the /Amazon EC2 User Guide/.
module Network.AWS.EC2.StopInstances
  ( -- * Creating a Request
    StopInstances (..),
    newStopInstances,

    -- * Request Lenses
    stopInstances_dryRun,
    stopInstances_force,
    stopInstances_hibernate,
    stopInstances_instanceIds,

    -- * Destructuring the Response
    StopInstancesResponse (..),
    newStopInstancesResponse,

    -- * Response Lenses
    stopInstancesResponse_stoppingInstances,
    stopInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopInstances' smart constructor.
data StopInstances = StopInstances'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Forces the instances to stop. The instances do not have an opportunity
    -- to flush file system caches or file system metadata. If you use this
    -- option, you must perform file system check and repair procedures. This
    -- option is not recommended for Windows instances.
    --
    -- Default: @false@
    force :: Prelude.Maybe Prelude.Bool,
    -- | Hibernates the instance if the instance was enabled for hibernation at
    -- launch. If the instance cannot hibernate successfully, a normal shutdown
    -- occurs. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance>
    -- in the /Amazon EC2 User Guide/.
    --
    -- Default: @false@
    hibernate :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the instances.
    instanceIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'stopInstances_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'force', 'stopInstances_force' - Forces the instances to stop. The instances do not have an opportunity
-- to flush file system caches or file system metadata. If you use this
-- option, you must perform file system check and repair procedures. This
-- option is not recommended for Windows instances.
--
-- Default: @false@
--
-- 'hibernate', 'stopInstances_hibernate' - Hibernates the instance if the instance was enabled for hibernation at
-- launch. If the instance cannot hibernate successfully, a normal shutdown
-- occurs. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance>
-- in the /Amazon EC2 User Guide/.
--
-- Default: @false@
--
-- 'instanceIds', 'stopInstances_instanceIds' - The IDs of the instances.
newStopInstances ::
  StopInstances
newStopInstances =
  StopInstances'
    { dryRun = Prelude.Nothing,
      force = Prelude.Nothing,
      hibernate = Prelude.Nothing,
      instanceIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
stopInstances_dryRun :: Lens.Lens' StopInstances (Prelude.Maybe Prelude.Bool)
stopInstances_dryRun = Lens.lens (\StopInstances' {dryRun} -> dryRun) (\s@StopInstances' {} a -> s {dryRun = a} :: StopInstances)

-- | Forces the instances to stop. The instances do not have an opportunity
-- to flush file system caches or file system metadata. If you use this
-- option, you must perform file system check and repair procedures. This
-- option is not recommended for Windows instances.
--
-- Default: @false@
stopInstances_force :: Lens.Lens' StopInstances (Prelude.Maybe Prelude.Bool)
stopInstances_force = Lens.lens (\StopInstances' {force} -> force) (\s@StopInstances' {} a -> s {force = a} :: StopInstances)

-- | Hibernates the instance if the instance was enabled for hibernation at
-- launch. If the instance cannot hibernate successfully, a normal shutdown
-- occurs. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Hibernate.html Hibernate your instance>
-- in the /Amazon EC2 User Guide/.
--
-- Default: @false@
stopInstances_hibernate :: Lens.Lens' StopInstances (Prelude.Maybe Prelude.Bool)
stopInstances_hibernate = Lens.lens (\StopInstances' {hibernate} -> hibernate) (\s@StopInstances' {} a -> s {hibernate = a} :: StopInstances)

-- | The IDs of the instances.
stopInstances_instanceIds :: Lens.Lens' StopInstances [Prelude.Text]
stopInstances_instanceIds = Lens.lens (\StopInstances' {instanceIds} -> instanceIds) (\s@StopInstances' {} a -> s {instanceIds = a} :: StopInstances) Prelude.. Lens._Coerce

instance Core.AWSRequest StopInstances where
  type
    AWSResponse StopInstances =
      StopInstancesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          StopInstancesResponse'
            Prelude.<$> ( x Core..@? "instancesSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopInstances

instance Prelude.NFData StopInstances

instance Core.ToHeaders StopInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath StopInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery StopInstances where
  toQuery StopInstances' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("StopInstances" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "Force" Core.=: force,
        "Hibernate" Core.=: hibernate,
        Core.toQueryList "InstanceId" instanceIds
      ]

-- | /See:/ 'newStopInstancesResponse' smart constructor.
data StopInstancesResponse = StopInstancesResponse'
  { -- | Information about the stopped instances.
    stoppingInstances :: Prelude.Maybe [InstanceStateChange],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stoppingInstances', 'stopInstancesResponse_stoppingInstances' - Information about the stopped instances.
--
-- 'httpStatus', 'stopInstancesResponse_httpStatus' - The response's http status code.
newStopInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopInstancesResponse
newStopInstancesResponse pHttpStatus_ =
  StopInstancesResponse'
    { stoppingInstances =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the stopped instances.
stopInstancesResponse_stoppingInstances :: Lens.Lens' StopInstancesResponse (Prelude.Maybe [InstanceStateChange])
stopInstancesResponse_stoppingInstances = Lens.lens (\StopInstancesResponse' {stoppingInstances} -> stoppingInstances) (\s@StopInstancesResponse' {} a -> s {stoppingInstances = a} :: StopInstancesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
stopInstancesResponse_httpStatus :: Lens.Lens' StopInstancesResponse Prelude.Int
stopInstancesResponse_httpStatus = Lens.lens (\StopInstancesResponse' {httpStatus} -> httpStatus) (\s@StopInstancesResponse' {} a -> s {httpStatus = a} :: StopInstancesResponse)

instance Prelude.NFData StopInstancesResponse
