{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptions
  ( LaunchTemplateInstanceMarketOptions (..),

    -- * Smart constructor
    mkLaunchTemplateInstanceMarketOptions,

    -- * Lenses
    ltimoMarketType,
    ltimoSpotOptions,
  )
where

import qualified Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptions as Types
import qualified Network.AWS.EC2.Types.MarketType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The market (purchasing) option for the instances.
--
-- /See:/ 'mkLaunchTemplateInstanceMarketOptions' smart constructor.
data LaunchTemplateInstanceMarketOptions = LaunchTemplateInstanceMarketOptions'
  { -- | The market type.
    marketType :: Core.Maybe Types.MarketType,
    -- | The options for Spot Instances.
    spotOptions :: Core.Maybe Types.LaunchTemplateSpotMarketOptions
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LaunchTemplateInstanceMarketOptions' value with any optional fields omitted.
mkLaunchTemplateInstanceMarketOptions ::
  LaunchTemplateInstanceMarketOptions
mkLaunchTemplateInstanceMarketOptions =
  LaunchTemplateInstanceMarketOptions'
    { marketType = Core.Nothing,
      spotOptions = Core.Nothing
    }

-- | The market type.
--
-- /Note:/ Consider using 'marketType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimoMarketType :: Lens.Lens' LaunchTemplateInstanceMarketOptions (Core.Maybe Types.MarketType)
ltimoMarketType = Lens.field @"marketType"
{-# DEPRECATED ltimoMarketType "Use generic-lens or generic-optics with 'marketType' instead." #-}

-- | The options for Spot Instances.
--
-- /Note:/ Consider using 'spotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimoSpotOptions :: Lens.Lens' LaunchTemplateInstanceMarketOptions (Core.Maybe Types.LaunchTemplateSpotMarketOptions)
ltimoSpotOptions = Lens.field @"spotOptions"
{-# DEPRECATED ltimoSpotOptions "Use generic-lens or generic-optics with 'spotOptions' instead." #-}

instance Core.FromXML LaunchTemplateInstanceMarketOptions where
  parseXML x =
    LaunchTemplateInstanceMarketOptions'
      Core.<$> (x Core..@? "marketType") Core.<*> (x Core..@? "spotOptions")
