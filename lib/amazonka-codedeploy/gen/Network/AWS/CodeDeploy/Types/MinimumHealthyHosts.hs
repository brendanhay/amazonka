{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.MinimumHealthyHosts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.MinimumHealthyHosts
  ( MinimumHealthyHosts (..),

    -- * Smart constructor
    mkMinimumHealthyHosts,

    -- * Lenses
    mhhValue,
    mhhType,
  )
where

import Network.AWS.CodeDeploy.Types.MinimumHealthyHostsType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about minimum healthy instance.
--
-- /See:/ 'mkMinimumHealthyHosts' smart constructor.
data MinimumHealthyHosts = MinimumHealthyHosts'
  { value ::
      Lude.Maybe Lude.Int,
    type' :: Lude.Maybe MinimumHealthyHostsType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MinimumHealthyHosts' with the minimum fields required to make a request.
--
-- * 'type'' - The minimum healthy instance type:
--
--
--     * @HOST_COUNT@ : The minimum number of healthy instances as an absolute value.
--
--
--     * @FLEET_PERCENT@ : The minimum number of healthy instances as a percentage of the total number of instances in the deployment.
--
--
-- In an example of nine instances, if a HOST_COUNT of six is specified, deploy to up to three instances at a time. The deployment is successful if six or more instances are deployed to successfully. Otherwise, the deployment fails. If a FLEET_PERCENT of 40 is specified, deploy to up to five instances at a time. The deployment is successful if four or more instances are deployed to successfully. Otherwise, the deployment fails.
-- For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/instances-health.html AWS CodeDeploy Instance Health> in the /AWS CodeDeploy User Guide/ .
-- * 'value' - The minimum healthy instance value.
mkMinimumHealthyHosts ::
  MinimumHealthyHosts
mkMinimumHealthyHosts =
  MinimumHealthyHosts' {value = Lude.Nothing, type' = Lude.Nothing}

-- | The minimum healthy instance value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhhValue :: Lens.Lens' MinimumHealthyHosts (Lude.Maybe Lude.Int)
mhhValue = Lens.lens (value :: MinimumHealthyHosts -> Lude.Maybe Lude.Int) (\s a -> s {value = a} :: MinimumHealthyHosts)
{-# DEPRECATED mhhValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The minimum healthy instance type:
--
--
--     * @HOST_COUNT@ : The minimum number of healthy instances as an absolute value.
--
--
--     * @FLEET_PERCENT@ : The minimum number of healthy instances as a percentage of the total number of instances in the deployment.
--
--
-- In an example of nine instances, if a HOST_COUNT of six is specified, deploy to up to three instances at a time. The deployment is successful if six or more instances are deployed to successfully. Otherwise, the deployment fails. If a FLEET_PERCENT of 40 is specified, deploy to up to five instances at a time. The deployment is successful if four or more instances are deployed to successfully. Otherwise, the deployment fails.
-- For more information, see <https://docs.aws.amazon.com/codedeploy/latest/userguide/instances-health.html AWS CodeDeploy Instance Health> in the /AWS CodeDeploy User Guide/ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhhType :: Lens.Lens' MinimumHealthyHosts (Lude.Maybe MinimumHealthyHostsType)
mhhType = Lens.lens (type' :: MinimumHealthyHosts -> Lude.Maybe MinimumHealthyHostsType) (\s a -> s {type' = a} :: MinimumHealthyHosts)
{-# DEPRECATED mhhType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON MinimumHealthyHosts where
  parseJSON =
    Lude.withObject
      "MinimumHealthyHosts"
      ( \x ->
          MinimumHealthyHosts'
            Lude.<$> (x Lude..:? "value") Lude.<*> (x Lude..:? "type")
      )

instance Lude.ToJSON MinimumHealthyHosts where
  toJSON MinimumHealthyHosts' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("value" Lude..=) Lude.<$> value,
            ("type" Lude..=) Lude.<$> type'
          ]
      )
