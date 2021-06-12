{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ScalingProcessQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.ScalingProcessQuery where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | /See:/ 'newScalingProcessQuery' smart constructor.
data ScalingProcessQuery = ScalingProcessQuery'
  { -- | One or more of the following processes:
    --
    -- -   @Launch@
    --
    -- -   @Terminate@
    --
    -- -   @AddToLoadBalancer@
    --
    -- -   @AlarmNotification@
    --
    -- -   @AZRebalance@
    --
    -- -   @HealthCheck@
    --
    -- -   @InstanceRefresh@
    --
    -- -   @ReplaceUnhealthy@
    --
    -- -   @ScheduledActions@
    --
    -- If you omit this parameter, all processes are specified.
    scalingProcesses :: Core.Maybe [Core.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScalingProcessQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scalingProcesses', 'scalingProcessQuery_scalingProcesses' - One or more of the following processes:
--
-- -   @Launch@
--
-- -   @Terminate@
--
-- -   @AddToLoadBalancer@
--
-- -   @AlarmNotification@
--
-- -   @AZRebalance@
--
-- -   @HealthCheck@
--
-- -   @InstanceRefresh@
--
-- -   @ReplaceUnhealthy@
--
-- -   @ScheduledActions@
--
-- If you omit this parameter, all processes are specified.
--
-- 'autoScalingGroupName', 'scalingProcessQuery_autoScalingGroupName' - The name of the Auto Scaling group.
newScalingProcessQuery ::
  -- | 'autoScalingGroupName'
  Core.Text ->
  ScalingProcessQuery
newScalingProcessQuery pAutoScalingGroupName_ =
  ScalingProcessQuery'
    { scalingProcesses =
        Core.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | One or more of the following processes:
--
-- -   @Launch@
--
-- -   @Terminate@
--
-- -   @AddToLoadBalancer@
--
-- -   @AlarmNotification@
--
-- -   @AZRebalance@
--
-- -   @HealthCheck@
--
-- -   @InstanceRefresh@
--
-- -   @ReplaceUnhealthy@
--
-- -   @ScheduledActions@
--
-- If you omit this parameter, all processes are specified.
scalingProcessQuery_scalingProcesses :: Lens.Lens' ScalingProcessQuery (Core.Maybe [Core.Text])
scalingProcessQuery_scalingProcesses = Lens.lens (\ScalingProcessQuery' {scalingProcesses} -> scalingProcesses) (\s@ScalingProcessQuery' {} a -> s {scalingProcesses = a} :: ScalingProcessQuery) Core.. Lens.mapping Lens._Coerce

-- | The name of the Auto Scaling group.
scalingProcessQuery_autoScalingGroupName :: Lens.Lens' ScalingProcessQuery Core.Text
scalingProcessQuery_autoScalingGroupName = Lens.lens (\ScalingProcessQuery' {autoScalingGroupName} -> autoScalingGroupName) (\s@ScalingProcessQuery' {} a -> s {autoScalingGroupName = a} :: ScalingProcessQuery)

instance Core.Hashable ScalingProcessQuery

instance Core.NFData ScalingProcessQuery

instance Core.ToQuery ScalingProcessQuery where
  toQuery ScalingProcessQuery' {..} =
    Core.mconcat
      [ "ScalingProcesses"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> scalingProcesses
            ),
        "AutoScalingGroupName" Core.=: autoScalingGroupName
      ]
