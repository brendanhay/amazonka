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
-- Module      : Network.AWS.AutoScaling.StartInstanceRefresh
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new instance refresh operation, which triggers a rolling
-- replacement of all previously launched instances in the Auto Scaling
-- group with a new group of instances.
--
-- If successful, this call creates a new instance refresh request with a
-- unique ID that you can use to track its progress. To query its status,
-- call the DescribeInstanceRefreshes API. To describe the instance
-- refreshes that have already run, call the DescribeInstanceRefreshes API.
-- To cancel an instance refresh operation in progress, use the
-- CancelInstanceRefresh API.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-refresh.html Replacing Auto Scaling Instances Based on an Instance Refresh>.
module Network.AWS.AutoScaling.StartInstanceRefresh
  ( -- * Creating a Request
    StartInstanceRefresh (..),
    newStartInstanceRefresh,

    -- * Request Lenses
    startInstanceRefresh_strategy,
    startInstanceRefresh_preferences,
    startInstanceRefresh_autoScalingGroupName,

    -- * Destructuring the Response
    StartInstanceRefreshResponse (..),
    newStartInstanceRefreshResponse,

    -- * Response Lenses
    startInstanceRefreshResponse_instanceRefreshId,
    startInstanceRefreshResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartInstanceRefresh' smart constructor.
data StartInstanceRefresh = StartInstanceRefresh'
  { -- | The strategy to use for the instance refresh. The only valid value is
    -- @Rolling@.
    --
    -- A rolling update is an update that is applied to all instances in an
    -- Auto Scaling group until all instances have been updated. A rolling
    -- update can fail due to failed health checks or if instances are on
    -- standby or are protected from scale in. If the rolling update process
    -- fails, any instances that were already replaced are not rolled back to
    -- their previous configuration.
    strategy :: Prelude.Maybe RefreshStrategy,
    -- | Set of preferences associated with the instance refresh request.
    --
    -- If not provided, the default values are used. For
    -- @MinHealthyPercentage@, the default value is @90@. For @InstanceWarmup@,
    -- the default is to use the value specified for the health check grace
    -- period for the Auto Scaling group.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_RefreshPreferences.html RefreshPreferences>
    -- in the /Amazon EC2 Auto Scaling API Reference/.
    preferences :: Prelude.Maybe RefreshPreferences,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartInstanceRefresh' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'strategy', 'startInstanceRefresh_strategy' - The strategy to use for the instance refresh. The only valid value is
-- @Rolling@.
--
-- A rolling update is an update that is applied to all instances in an
-- Auto Scaling group until all instances have been updated. A rolling
-- update can fail due to failed health checks or if instances are on
-- standby or are protected from scale in. If the rolling update process
-- fails, any instances that were already replaced are not rolled back to
-- their previous configuration.
--
-- 'preferences', 'startInstanceRefresh_preferences' - Set of preferences associated with the instance refresh request.
--
-- If not provided, the default values are used. For
-- @MinHealthyPercentage@, the default value is @90@. For @InstanceWarmup@,
-- the default is to use the value specified for the health check grace
-- period for the Auto Scaling group.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_RefreshPreferences.html RefreshPreferences>
-- in the /Amazon EC2 Auto Scaling API Reference/.
--
-- 'autoScalingGroupName', 'startInstanceRefresh_autoScalingGroupName' - The name of the Auto Scaling group.
newStartInstanceRefresh ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  StartInstanceRefresh
newStartInstanceRefresh pAutoScalingGroupName_ =
  StartInstanceRefresh'
    { strategy = Prelude.Nothing,
      preferences = Prelude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The strategy to use for the instance refresh. The only valid value is
-- @Rolling@.
--
-- A rolling update is an update that is applied to all instances in an
-- Auto Scaling group until all instances have been updated. A rolling
-- update can fail due to failed health checks or if instances are on
-- standby or are protected from scale in. If the rolling update process
-- fails, any instances that were already replaced are not rolled back to
-- their previous configuration.
startInstanceRefresh_strategy :: Lens.Lens' StartInstanceRefresh (Prelude.Maybe RefreshStrategy)
startInstanceRefresh_strategy = Lens.lens (\StartInstanceRefresh' {strategy} -> strategy) (\s@StartInstanceRefresh' {} a -> s {strategy = a} :: StartInstanceRefresh)

-- | Set of preferences associated with the instance refresh request.
--
-- If not provided, the default values are used. For
-- @MinHealthyPercentage@, the default value is @90@. For @InstanceWarmup@,
-- the default is to use the value specified for the health check grace
-- period for the Auto Scaling group.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_RefreshPreferences.html RefreshPreferences>
-- in the /Amazon EC2 Auto Scaling API Reference/.
startInstanceRefresh_preferences :: Lens.Lens' StartInstanceRefresh (Prelude.Maybe RefreshPreferences)
startInstanceRefresh_preferences = Lens.lens (\StartInstanceRefresh' {preferences} -> preferences) (\s@StartInstanceRefresh' {} a -> s {preferences = a} :: StartInstanceRefresh)

-- | The name of the Auto Scaling group.
startInstanceRefresh_autoScalingGroupName :: Lens.Lens' StartInstanceRefresh Prelude.Text
startInstanceRefresh_autoScalingGroupName = Lens.lens (\StartInstanceRefresh' {autoScalingGroupName} -> autoScalingGroupName) (\s@StartInstanceRefresh' {} a -> s {autoScalingGroupName = a} :: StartInstanceRefresh)

instance Prelude.AWSRequest StartInstanceRefresh where
  type
    Rs StartInstanceRefresh =
      StartInstanceRefreshResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "StartInstanceRefreshResult"
      ( \s h x ->
          StartInstanceRefreshResponse'
            Prelude.<$> (x Prelude..@? "InstanceRefreshId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartInstanceRefresh

instance Prelude.NFData StartInstanceRefresh

instance Prelude.ToHeaders StartInstanceRefresh where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath StartInstanceRefresh where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartInstanceRefresh where
  toQuery StartInstanceRefresh' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("StartInstanceRefresh" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2011-01-01" :: Prelude.ByteString),
        "Strategy" Prelude.=: strategy,
        "Preferences" Prelude.=: preferences,
        "AutoScalingGroupName"
          Prelude.=: autoScalingGroupName
      ]

-- | /See:/ 'newStartInstanceRefreshResponse' smart constructor.
data StartInstanceRefreshResponse = StartInstanceRefreshResponse'
  { -- | A unique ID for tracking the progress of the request.
    instanceRefreshId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartInstanceRefreshResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceRefreshId', 'startInstanceRefreshResponse_instanceRefreshId' - A unique ID for tracking the progress of the request.
--
-- 'httpStatus', 'startInstanceRefreshResponse_httpStatus' - The response's http status code.
newStartInstanceRefreshResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartInstanceRefreshResponse
newStartInstanceRefreshResponse pHttpStatus_ =
  StartInstanceRefreshResponse'
    { instanceRefreshId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique ID for tracking the progress of the request.
startInstanceRefreshResponse_instanceRefreshId :: Lens.Lens' StartInstanceRefreshResponse (Prelude.Maybe Prelude.Text)
startInstanceRefreshResponse_instanceRefreshId = Lens.lens (\StartInstanceRefreshResponse' {instanceRefreshId} -> instanceRefreshId) (\s@StartInstanceRefreshResponse' {} a -> s {instanceRefreshId = a} :: StartInstanceRefreshResponse)

-- | The response's http status code.
startInstanceRefreshResponse_httpStatus :: Lens.Lens' StartInstanceRefreshResponse Prelude.Int
startInstanceRefreshResponse_httpStatus = Lens.lens (\StartInstanceRefreshResponse' {httpStatus} -> httpStatus) (\s@StartInstanceRefreshResponse' {} a -> s {httpStatus = a} :: StartInstanceRefreshResponse)

instance Prelude.NFData StartInstanceRefreshResponse
