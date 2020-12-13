{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.GreenFleetProvisioningOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.GreenFleetProvisioningOption
  ( GreenFleetProvisioningOption (..),

    -- * Smart constructor
    mkGreenFleetProvisioningOption,

    -- * Lenses
    gfpoAction,
  )
where

import Network.AWS.CodeDeploy.Types.GreenFleetProvisioningAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the instances that belong to the replacement environment in a blue/green deployment.
--
-- /See:/ 'mkGreenFleetProvisioningOption' smart constructor.
newtype GreenFleetProvisioningOption = GreenFleetProvisioningOption'
  { -- | The method used to add instances to a replacement environment.
    --
    --
    --     * @DISCOVER_EXISTING@ : Use instances that already exist or will be created manually.
    --
    --
    --     * @COPY_AUTO_SCALING_GROUP@ : Use settings from a specified Auto Scaling group to define and create instances in a new Auto Scaling group.
    action :: Lude.Maybe GreenFleetProvisioningAction
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GreenFleetProvisioningOption' with the minimum fields required to make a request.
--
-- * 'action' - The method used to add instances to a replacement environment.
--
--
--     * @DISCOVER_EXISTING@ : Use instances that already exist or will be created manually.
--
--
--     * @COPY_AUTO_SCALING_GROUP@ : Use settings from a specified Auto Scaling group to define and create instances in a new Auto Scaling group.
mkGreenFleetProvisioningOption ::
  GreenFleetProvisioningOption
mkGreenFleetProvisioningOption =
  GreenFleetProvisioningOption' {action = Lude.Nothing}

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
gfpoAction :: Lens.Lens' GreenFleetProvisioningOption (Lude.Maybe GreenFleetProvisioningAction)
gfpoAction = Lens.lens (action :: GreenFleetProvisioningOption -> Lude.Maybe GreenFleetProvisioningAction) (\s a -> s {action = a} :: GreenFleetProvisioningOption)
{-# DEPRECATED gfpoAction "Use generic-lens or generic-optics with 'action' instead." #-}

instance Lude.FromJSON GreenFleetProvisioningOption where
  parseJSON =
    Lude.withObject
      "GreenFleetProvisioningOption"
      ( \x ->
          GreenFleetProvisioningOption' Lude.<$> (x Lude..:? "action")
      )

instance Lude.ToJSON GreenFleetProvisioningOption where
  toJSON GreenFleetProvisioningOption' {..} =
    Lude.object (Lude.catMaybes [("action" Lude..=) Lude.<$> action])
