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
-- Module      : Network.AWS.AutoScaling.SetInstanceHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the health status of the specified instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health checks for Auto Scaling instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
module Network.AWS.AutoScaling.SetInstanceHealth
  ( -- * Creating a Request
    SetInstanceHealth (..),
    newSetInstanceHealth,

    -- * Request Lenses
    setInstanceHealth_shouldRespectGracePeriod,
    setInstanceHealth_instanceId,
    setInstanceHealth_healthStatus,

    -- * Destructuring the Response
    SetInstanceHealthResponse (..),
    newSetInstanceHealthResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetInstanceHealth' smart constructor.
data SetInstanceHealth = SetInstanceHealth'
  { -- | If the Auto Scaling group of the specified instance has a
    -- @HealthCheckGracePeriod@ specified for the group, by default, this call
    -- respects the grace period. Set this to @False@, to have the call not
    -- respect the grace period associated with the group.
    --
    -- For more information about the health check grace period, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_CreateAutoScalingGroup.html CreateAutoScalingGroup>
    -- in the /Amazon EC2 Auto Scaling API Reference/.
    shouldRespectGracePeriod :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the instance.
    instanceId :: Prelude.Text,
    -- | The health status of the instance. Set to @Healthy@ to have the instance
    -- remain in service. Set to @Unhealthy@ to have the instance be out of
    -- service. Amazon EC2 Auto Scaling terminates and replaces the unhealthy
    -- instance.
    healthStatus :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetInstanceHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shouldRespectGracePeriod', 'setInstanceHealth_shouldRespectGracePeriod' - If the Auto Scaling group of the specified instance has a
-- @HealthCheckGracePeriod@ specified for the group, by default, this call
-- respects the grace period. Set this to @False@, to have the call not
-- respect the grace period associated with the group.
--
-- For more information about the health check grace period, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_CreateAutoScalingGroup.html CreateAutoScalingGroup>
-- in the /Amazon EC2 Auto Scaling API Reference/.
--
-- 'instanceId', 'setInstanceHealth_instanceId' - The ID of the instance.
--
-- 'healthStatus', 'setInstanceHealth_healthStatus' - The health status of the instance. Set to @Healthy@ to have the instance
-- remain in service. Set to @Unhealthy@ to have the instance be out of
-- service. Amazon EC2 Auto Scaling terminates and replaces the unhealthy
-- instance.
newSetInstanceHealth ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'healthStatus'
  Prelude.Text ->
  SetInstanceHealth
newSetInstanceHealth pInstanceId_ pHealthStatus_ =
  SetInstanceHealth'
    { shouldRespectGracePeriod =
        Prelude.Nothing,
      instanceId = pInstanceId_,
      healthStatus = pHealthStatus_
    }

-- | If the Auto Scaling group of the specified instance has a
-- @HealthCheckGracePeriod@ specified for the group, by default, this call
-- respects the grace period. Set this to @False@, to have the call not
-- respect the grace period associated with the group.
--
-- For more information about the health check grace period, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_CreateAutoScalingGroup.html CreateAutoScalingGroup>
-- in the /Amazon EC2 Auto Scaling API Reference/.
setInstanceHealth_shouldRespectGracePeriod :: Lens.Lens' SetInstanceHealth (Prelude.Maybe Prelude.Bool)
setInstanceHealth_shouldRespectGracePeriod = Lens.lens (\SetInstanceHealth' {shouldRespectGracePeriod} -> shouldRespectGracePeriod) (\s@SetInstanceHealth' {} a -> s {shouldRespectGracePeriod = a} :: SetInstanceHealth)

-- | The ID of the instance.
setInstanceHealth_instanceId :: Lens.Lens' SetInstanceHealth Prelude.Text
setInstanceHealth_instanceId = Lens.lens (\SetInstanceHealth' {instanceId} -> instanceId) (\s@SetInstanceHealth' {} a -> s {instanceId = a} :: SetInstanceHealth)

-- | The health status of the instance. Set to @Healthy@ to have the instance
-- remain in service. Set to @Unhealthy@ to have the instance be out of
-- service. Amazon EC2 Auto Scaling terminates and replaces the unhealthy
-- instance.
setInstanceHealth_healthStatus :: Lens.Lens' SetInstanceHealth Prelude.Text
setInstanceHealth_healthStatus = Lens.lens (\SetInstanceHealth' {healthStatus} -> healthStatus) (\s@SetInstanceHealth' {} a -> s {healthStatus = a} :: SetInstanceHealth)

instance Prelude.AWSRequest SetInstanceHealth where
  type Rs SetInstanceHealth = SetInstanceHealthResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull SetInstanceHealthResponse'

instance Prelude.Hashable SetInstanceHealth

instance Prelude.NFData SetInstanceHealth

instance Prelude.ToHeaders SetInstanceHealth where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath SetInstanceHealth where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SetInstanceHealth where
  toQuery SetInstanceHealth' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("SetInstanceHealth" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2011-01-01" :: Prelude.ByteString),
        "ShouldRespectGracePeriod"
          Prelude.=: shouldRespectGracePeriod,
        "InstanceId" Prelude.=: instanceId,
        "HealthStatus" Prelude.=: healthStatus
      ]

-- | /See:/ 'newSetInstanceHealthResponse' smart constructor.
data SetInstanceHealthResponse = SetInstanceHealthResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetInstanceHealthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetInstanceHealthResponse ::
  SetInstanceHealthResponse
newSetInstanceHealthResponse =
  SetInstanceHealthResponse'

instance Prelude.NFData SetInstanceHealthResponse
