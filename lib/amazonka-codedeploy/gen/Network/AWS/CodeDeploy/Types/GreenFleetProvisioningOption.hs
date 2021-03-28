{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.GreenFleetProvisioningOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.GreenFleetProvisioningOption
  ( GreenFleetProvisioningOption (..)
  -- * Smart constructor
  , mkGreenFleetProvisioningOption
  -- * Lenses
  , gfpoAction
  ) where

import qualified Network.AWS.CodeDeploy.Types.GreenFleetProvisioningAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the instances that belong to the replacement environment in a blue/green deployment.
--
-- /See:/ 'mkGreenFleetProvisioningOption' smart constructor.
newtype GreenFleetProvisioningOption = GreenFleetProvisioningOption'
  { action :: Core.Maybe Types.GreenFleetProvisioningAction
    -- ^ The method used to add instances to a replacement environment.
--
--
--     * @DISCOVER_EXISTING@ : Use instances that already exist or will be created manually.
--
--
--     * @COPY_AUTO_SCALING_GROUP@ : Use settings from a specified Auto Scaling group to define and create instances in a new Auto Scaling group.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GreenFleetProvisioningOption' value with any optional fields omitted.
mkGreenFleetProvisioningOption
    :: GreenFleetProvisioningOption
mkGreenFleetProvisioningOption
  = GreenFleetProvisioningOption'{action = Core.Nothing}

-- | The method used to add instances to a replacement environment.
--
--
--     * @DISCOVER_EXISTING@ : Use instances that already exist or will be created manually.
--
--
--     * @COPY_AUTO_SCALING_GROUP@ : Use settings from a specified Auto Scaling group to define and create instances in a new Auto Scaling group.
--
--
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfpoAction :: Lens.Lens' GreenFleetProvisioningOption (Core.Maybe Types.GreenFleetProvisioningAction)
gfpoAction = Lens.field @"action"
{-# INLINEABLE gfpoAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

instance Core.FromJSON GreenFleetProvisioningOption where
        toJSON GreenFleetProvisioningOption{..}
          = Core.object (Core.catMaybes [("action" Core..=) Core.<$> action])

instance Core.FromJSON GreenFleetProvisioningOption where
        parseJSON
          = Core.withObject "GreenFleetProvisioningOption" Core.$
              \ x -> GreenFleetProvisioningOption' Core.<$> (x Core..:? "action")
