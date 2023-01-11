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
-- Module      : Amazonka.AutoScaling.StartInstanceRefresh
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new instance refresh operation. An instance refresh performs a
-- rolling replacement of all or some instances in an Auto Scaling group.
-- Each instance is terminated first and then replaced, which temporarily
-- reduces the capacity available within your Auto Scaling group.
--
-- This operation is part of the
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-refresh.html instance refresh feature>
-- in Amazon EC2 Auto Scaling, which helps you update instances in your
-- Auto Scaling group. This feature is helpful, for example, when you have
-- a new AMI or a new user data script. You just need to create a new
-- launch template that specifies the new AMI or user data script. Then
-- start an instance refresh to immediately begin the process of updating
-- instances in the group.
--
-- If the call succeeds, it creates a new instance refresh request with a
-- unique ID that you can use to track its progress. To query its status,
-- call the DescribeInstanceRefreshes API. To describe the instance
-- refreshes that have already run, call the DescribeInstanceRefreshes API.
-- To cancel an instance refresh operation in progress, use the
-- CancelInstanceRefresh API.
module Amazonka.AutoScaling.StartInstanceRefresh
  ( -- * Creating a Request
    StartInstanceRefresh (..),
    newStartInstanceRefresh,

    -- * Request Lenses
    startInstanceRefresh_desiredConfiguration,
    startInstanceRefresh_preferences,
    startInstanceRefresh_strategy,
    startInstanceRefresh_autoScalingGroupName,

    -- * Destructuring the Response
    StartInstanceRefreshResponse (..),
    newStartInstanceRefreshResponse,

    -- * Response Lenses
    startInstanceRefreshResponse_instanceRefreshId,
    startInstanceRefreshResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartInstanceRefresh' smart constructor.
data StartInstanceRefresh = StartInstanceRefresh'
  { -- | The desired configuration. For example, the desired configuration can
    -- specify a new launch template or a new version of the current launch
    -- template.
    --
    -- Once the instance refresh succeeds, Amazon EC2 Auto Scaling updates the
    -- settings of the Auto Scaling group to reflect the new desired
    -- configuration.
    --
    -- When you specify a new launch template or a new version of the current
    -- launch template for your desired configuration, consider enabling the
    -- @SkipMatching@ property in preferences. If it\'s enabled, Amazon EC2
    -- Auto Scaling skips replacing instances that already use the specified
    -- launch template and version. This can help you reduce the number of
    -- replacements that are required to apply updates.
    desiredConfiguration :: Prelude.Maybe DesiredConfiguration,
    -- | Set of preferences associated with the instance refresh request. If not
    -- provided, the default values are used.
    preferences :: Prelude.Maybe RefreshPreferences,
    -- | The strategy to use for the instance refresh. The only valid value is
    -- @Rolling@.
    --
    -- A rolling update helps you update your instances gradually. A rolling
    -- update can fail due to failed health checks or if instances are on
    -- standby or are protected from scale in. If the rolling update process
    -- fails, any instances that are replaced are not rolled back to their
    -- previous configuration.
    strategy :: Prelude.Maybe RefreshStrategy,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartInstanceRefresh' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredConfiguration', 'startInstanceRefresh_desiredConfiguration' - The desired configuration. For example, the desired configuration can
-- specify a new launch template or a new version of the current launch
-- template.
--
-- Once the instance refresh succeeds, Amazon EC2 Auto Scaling updates the
-- settings of the Auto Scaling group to reflect the new desired
-- configuration.
--
-- When you specify a new launch template or a new version of the current
-- launch template for your desired configuration, consider enabling the
-- @SkipMatching@ property in preferences. If it\'s enabled, Amazon EC2
-- Auto Scaling skips replacing instances that already use the specified
-- launch template and version. This can help you reduce the number of
-- replacements that are required to apply updates.
--
-- 'preferences', 'startInstanceRefresh_preferences' - Set of preferences associated with the instance refresh request. If not
-- provided, the default values are used.
--
-- 'strategy', 'startInstanceRefresh_strategy' - The strategy to use for the instance refresh. The only valid value is
-- @Rolling@.
--
-- A rolling update helps you update your instances gradually. A rolling
-- update can fail due to failed health checks or if instances are on
-- standby or are protected from scale in. If the rolling update process
-- fails, any instances that are replaced are not rolled back to their
-- previous configuration.
--
-- 'autoScalingGroupName', 'startInstanceRefresh_autoScalingGroupName' - The name of the Auto Scaling group.
newStartInstanceRefresh ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  StartInstanceRefresh
newStartInstanceRefresh pAutoScalingGroupName_ =
  StartInstanceRefresh'
    { desiredConfiguration =
        Prelude.Nothing,
      preferences = Prelude.Nothing,
      strategy = Prelude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The desired configuration. For example, the desired configuration can
-- specify a new launch template or a new version of the current launch
-- template.
--
-- Once the instance refresh succeeds, Amazon EC2 Auto Scaling updates the
-- settings of the Auto Scaling group to reflect the new desired
-- configuration.
--
-- When you specify a new launch template or a new version of the current
-- launch template for your desired configuration, consider enabling the
-- @SkipMatching@ property in preferences. If it\'s enabled, Amazon EC2
-- Auto Scaling skips replacing instances that already use the specified
-- launch template and version. This can help you reduce the number of
-- replacements that are required to apply updates.
startInstanceRefresh_desiredConfiguration :: Lens.Lens' StartInstanceRefresh (Prelude.Maybe DesiredConfiguration)
startInstanceRefresh_desiredConfiguration = Lens.lens (\StartInstanceRefresh' {desiredConfiguration} -> desiredConfiguration) (\s@StartInstanceRefresh' {} a -> s {desiredConfiguration = a} :: StartInstanceRefresh)

-- | Set of preferences associated with the instance refresh request. If not
-- provided, the default values are used.
startInstanceRefresh_preferences :: Lens.Lens' StartInstanceRefresh (Prelude.Maybe RefreshPreferences)
startInstanceRefresh_preferences = Lens.lens (\StartInstanceRefresh' {preferences} -> preferences) (\s@StartInstanceRefresh' {} a -> s {preferences = a} :: StartInstanceRefresh)

-- | The strategy to use for the instance refresh. The only valid value is
-- @Rolling@.
--
-- A rolling update helps you update your instances gradually. A rolling
-- update can fail due to failed health checks or if instances are on
-- standby or are protected from scale in. If the rolling update process
-- fails, any instances that are replaced are not rolled back to their
-- previous configuration.
startInstanceRefresh_strategy :: Lens.Lens' StartInstanceRefresh (Prelude.Maybe RefreshStrategy)
startInstanceRefresh_strategy = Lens.lens (\StartInstanceRefresh' {strategy} -> strategy) (\s@StartInstanceRefresh' {} a -> s {strategy = a} :: StartInstanceRefresh)

-- | The name of the Auto Scaling group.
startInstanceRefresh_autoScalingGroupName :: Lens.Lens' StartInstanceRefresh Prelude.Text
startInstanceRefresh_autoScalingGroupName = Lens.lens (\StartInstanceRefresh' {autoScalingGroupName} -> autoScalingGroupName) (\s@StartInstanceRefresh' {} a -> s {autoScalingGroupName = a} :: StartInstanceRefresh)

instance Core.AWSRequest StartInstanceRefresh where
  type
    AWSResponse StartInstanceRefresh =
      StartInstanceRefreshResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "StartInstanceRefreshResult"
      ( \s h x ->
          StartInstanceRefreshResponse'
            Prelude.<$> (x Data..@? "InstanceRefreshId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartInstanceRefresh where
  hashWithSalt _salt StartInstanceRefresh' {..} =
    _salt `Prelude.hashWithSalt` desiredConfiguration
      `Prelude.hashWithSalt` preferences
      `Prelude.hashWithSalt` strategy
      `Prelude.hashWithSalt` autoScalingGroupName

instance Prelude.NFData StartInstanceRefresh where
  rnf StartInstanceRefresh' {..} =
    Prelude.rnf desiredConfiguration
      `Prelude.seq` Prelude.rnf preferences
      `Prelude.seq` Prelude.rnf strategy
      `Prelude.seq` Prelude.rnf autoScalingGroupName

instance Data.ToHeaders StartInstanceRefresh where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath StartInstanceRefresh where
  toPath = Prelude.const "/"

instance Data.ToQuery StartInstanceRefresh where
  toQuery StartInstanceRefresh' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("StartInstanceRefresh" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "DesiredConfiguration" Data.=: desiredConfiguration,
        "Preferences" Data.=: preferences,
        "Strategy" Data.=: strategy,
        "AutoScalingGroupName" Data.=: autoScalingGroupName
      ]

-- | /See:/ 'newStartInstanceRefreshResponse' smart constructor.
data StartInstanceRefreshResponse = StartInstanceRefreshResponse'
  { -- | A unique ID for tracking the progress of the request.
    instanceRefreshId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData StartInstanceRefreshResponse where
  rnf StartInstanceRefreshResponse' {..} =
    Prelude.rnf instanceRefreshId
      `Prelude.seq` Prelude.rnf httpStatus
