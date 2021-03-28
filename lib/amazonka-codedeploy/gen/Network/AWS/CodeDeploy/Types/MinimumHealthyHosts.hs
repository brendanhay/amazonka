{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.MinimumHealthyHosts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.MinimumHealthyHosts
  ( MinimumHealthyHosts (..)
  -- * Smart constructor
  , mkMinimumHealthyHosts
  -- * Lenses
  , mhhType
  , mhhValue
  ) where

import qualified Network.AWS.CodeDeploy.Types.MinimumHealthyHostsType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about minimum healthy instance.
--
-- /See:/ 'mkMinimumHealthyHosts' smart constructor.
data MinimumHealthyHosts = MinimumHealthyHosts'
  { type' :: Core.Maybe Types.MinimumHealthyHostsType
    -- ^ The minimum healthy instance type:
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
  , value :: Core.Maybe Core.Int
    -- ^ The minimum healthy instance value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MinimumHealthyHosts' value with any optional fields omitted.
mkMinimumHealthyHosts
    :: MinimumHealthyHosts
mkMinimumHealthyHosts
  = MinimumHealthyHosts'{type' = Core.Nothing, value = Core.Nothing}

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
mhhType :: Lens.Lens' MinimumHealthyHosts (Core.Maybe Types.MinimumHealthyHostsType)
mhhType = Lens.field @"type'"
{-# INLINEABLE mhhType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The minimum healthy instance value.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mhhValue :: Lens.Lens' MinimumHealthyHosts (Core.Maybe Core.Int)
mhhValue = Lens.field @"value"
{-# INLINEABLE mhhValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON MinimumHealthyHosts where
        toJSON MinimumHealthyHosts{..}
          = Core.object
              (Core.catMaybes
                 [("type" Core..=) Core.<$> type',
                  ("value" Core..=) Core.<$> value])

instance Core.FromJSON MinimumHealthyHosts where
        parseJSON
          = Core.withObject "MinimumHealthyHosts" Core.$
              \ x ->
                MinimumHealthyHosts' Core.<$>
                  (x Core..:? "type") Core.<*> x Core..:? "value"
