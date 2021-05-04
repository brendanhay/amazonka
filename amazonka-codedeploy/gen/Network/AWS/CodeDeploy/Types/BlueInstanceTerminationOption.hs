{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeDeploy.Types.BlueInstanceTerminationOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.BlueInstanceTerminationOption where

import Network.AWS.CodeDeploy.Types.InstanceAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about whether instances in the original environment are
-- terminated when a blue\/green deployment is successful.
-- @BlueInstanceTerminationOption@ does not apply to Lambda deployments.
--
-- /See:/ 'newBlueInstanceTerminationOption' smart constructor.
data BlueInstanceTerminationOption = BlueInstanceTerminationOption'
  { -- | The action to take on instances in the original environment after a
    -- successful blue\/green deployment.
    --
    -- -   @TERMINATE@: Instances are terminated after a specified wait time.
    --
    -- -   @KEEP_ALIVE@: Instances are left running after they are deregistered
    --     from the load balancer and removed from the deployment group.
    action :: Prelude.Maybe InstanceAction,
    -- | For an Amazon EC2 deployment, the number of minutes to wait after a
    -- successful blue\/green deployment before terminating instances from the
    -- original environment.
    --
    -- For an Amazon ECS deployment, the number of minutes before deleting the
    -- original (blue) task set. During an Amazon ECS deployment, CodeDeploy
    -- shifts traffic from the original (blue) task set to a replacement
    -- (green) task set.
    --
    -- The maximum setting is 2880 minutes (2 days).
    terminationWaitTimeInMinutes :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BlueInstanceTerminationOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'blueInstanceTerminationOption_action' - The action to take on instances in the original environment after a
-- successful blue\/green deployment.
--
-- -   @TERMINATE@: Instances are terminated after a specified wait time.
--
-- -   @KEEP_ALIVE@: Instances are left running after they are deregistered
--     from the load balancer and removed from the deployment group.
--
-- 'terminationWaitTimeInMinutes', 'blueInstanceTerminationOption_terminationWaitTimeInMinutes' - For an Amazon EC2 deployment, the number of minutes to wait after a
-- successful blue\/green deployment before terminating instances from the
-- original environment.
--
-- For an Amazon ECS deployment, the number of minutes before deleting the
-- original (blue) task set. During an Amazon ECS deployment, CodeDeploy
-- shifts traffic from the original (blue) task set to a replacement
-- (green) task set.
--
-- The maximum setting is 2880 minutes (2 days).
newBlueInstanceTerminationOption ::
  BlueInstanceTerminationOption
newBlueInstanceTerminationOption =
  BlueInstanceTerminationOption'
    { action =
        Prelude.Nothing,
      terminationWaitTimeInMinutes =
        Prelude.Nothing
    }

-- | The action to take on instances in the original environment after a
-- successful blue\/green deployment.
--
-- -   @TERMINATE@: Instances are terminated after a specified wait time.
--
-- -   @KEEP_ALIVE@: Instances are left running after they are deregistered
--     from the load balancer and removed from the deployment group.
blueInstanceTerminationOption_action :: Lens.Lens' BlueInstanceTerminationOption (Prelude.Maybe InstanceAction)
blueInstanceTerminationOption_action = Lens.lens (\BlueInstanceTerminationOption' {action} -> action) (\s@BlueInstanceTerminationOption' {} a -> s {action = a} :: BlueInstanceTerminationOption)

-- | For an Amazon EC2 deployment, the number of minutes to wait after a
-- successful blue\/green deployment before terminating instances from the
-- original environment.
--
-- For an Amazon ECS deployment, the number of minutes before deleting the
-- original (blue) task set. During an Amazon ECS deployment, CodeDeploy
-- shifts traffic from the original (blue) task set to a replacement
-- (green) task set.
--
-- The maximum setting is 2880 minutes (2 days).
blueInstanceTerminationOption_terminationWaitTimeInMinutes :: Lens.Lens' BlueInstanceTerminationOption (Prelude.Maybe Prelude.Int)
blueInstanceTerminationOption_terminationWaitTimeInMinutes = Lens.lens (\BlueInstanceTerminationOption' {terminationWaitTimeInMinutes} -> terminationWaitTimeInMinutes) (\s@BlueInstanceTerminationOption' {} a -> s {terminationWaitTimeInMinutes = a} :: BlueInstanceTerminationOption)

instance
  Prelude.FromJSON
    BlueInstanceTerminationOption
  where
  parseJSON =
    Prelude.withObject
      "BlueInstanceTerminationOption"
      ( \x ->
          BlueInstanceTerminationOption'
            Prelude.<$> (x Prelude..:? "action")
            Prelude.<*> (x Prelude..:? "terminationWaitTimeInMinutes")
      )

instance
  Prelude.Hashable
    BlueInstanceTerminationOption

instance Prelude.NFData BlueInstanceTerminationOption

instance Prelude.ToJSON BlueInstanceTerminationOption where
  toJSON BlueInstanceTerminationOption' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("action" Prelude..=) Prelude.<$> action,
            ("terminationWaitTimeInMinutes" Prelude..=)
              Prelude.<$> terminationWaitTimeInMinutes
          ]
      )
