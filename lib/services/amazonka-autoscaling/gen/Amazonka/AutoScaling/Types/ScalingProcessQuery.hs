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
-- Module      : Amazonka.AutoScaling.Types.ScalingProcessQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.ScalingProcessQuery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
    -- If you omit this property, all processes are specified.
    scalingProcesses :: Prelude.Maybe [Prelude.Text],
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- If you omit this property, all processes are specified.
--
-- 'autoScalingGroupName', 'scalingProcessQuery_autoScalingGroupName' - The name of the Auto Scaling group.
newScalingProcessQuery ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  ScalingProcessQuery
newScalingProcessQuery pAutoScalingGroupName_ =
  ScalingProcessQuery'
    { scalingProcesses =
        Prelude.Nothing,
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
-- If you omit this property, all processes are specified.
scalingProcessQuery_scalingProcesses :: Lens.Lens' ScalingProcessQuery (Prelude.Maybe [Prelude.Text])
scalingProcessQuery_scalingProcesses = Lens.lens (\ScalingProcessQuery' {scalingProcesses} -> scalingProcesses) (\s@ScalingProcessQuery' {} a -> s {scalingProcesses = a} :: ScalingProcessQuery) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Auto Scaling group.
scalingProcessQuery_autoScalingGroupName :: Lens.Lens' ScalingProcessQuery Prelude.Text
scalingProcessQuery_autoScalingGroupName = Lens.lens (\ScalingProcessQuery' {autoScalingGroupName} -> autoScalingGroupName) (\s@ScalingProcessQuery' {} a -> s {autoScalingGroupName = a} :: ScalingProcessQuery)

instance Prelude.Hashable ScalingProcessQuery where
  hashWithSalt _salt ScalingProcessQuery' {..} =
    _salt
      `Prelude.hashWithSalt` scalingProcesses
      `Prelude.hashWithSalt` autoScalingGroupName

instance Prelude.NFData ScalingProcessQuery where
  rnf ScalingProcessQuery' {..} =
    Prelude.rnf scalingProcesses
      `Prelude.seq` Prelude.rnf autoScalingGroupName

instance Data.ToQuery ScalingProcessQuery where
  toQuery ScalingProcessQuery' {..} =
    Prelude.mconcat
      [ "ScalingProcesses"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> scalingProcesses
            ),
        "AutoScalingGroupName" Data.=: autoScalingGroupName
      ]
