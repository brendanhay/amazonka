{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.NetworkConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.NetworkConfiguration
  ( NetworkConfiguration (..),

    -- * Smart constructor
    mkNetworkConfiguration,

    -- * Lenses
    ncAwsvpcConfiguration,
  )
where

import qualified Network.AWS.ECS.Types.AwsVpcConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing the network configuration for a task or service.
--
-- /See:/ 'mkNetworkConfiguration' smart constructor.
newtype NetworkConfiguration = NetworkConfiguration'
  { -- | The VPC subnets and security groups associated with a task.
    awsvpcConfiguration :: Core.Maybe Types.AwsVpcConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkConfiguration' value with any optional fields omitted.
mkNetworkConfiguration ::
  NetworkConfiguration
mkNetworkConfiguration =
  NetworkConfiguration' {awsvpcConfiguration = Core.Nothing}

-- | The VPC subnets and security groups associated with a task.
--
-- /Note:/ Consider using 'awsvpcConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncAwsvpcConfiguration :: Lens.Lens' NetworkConfiguration (Core.Maybe Types.AwsVpcConfiguration)
ncAwsvpcConfiguration = Lens.field @"awsvpcConfiguration"
{-# DEPRECATED ncAwsvpcConfiguration "Use generic-lens or generic-optics with 'awsvpcConfiguration' instead." #-}

instance Core.FromJSON NetworkConfiguration where
  toJSON NetworkConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [("awsvpcConfiguration" Core..=) Core.<$> awsvpcConfiguration]
      )

instance Core.FromJSON NetworkConfiguration where
  parseJSON =
    Core.withObject "NetworkConfiguration" Core.$
      \x ->
        NetworkConfiguration' Core.<$> (x Core..:? "awsvpcConfiguration")
