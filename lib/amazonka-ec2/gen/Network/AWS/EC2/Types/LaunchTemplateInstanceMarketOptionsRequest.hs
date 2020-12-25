{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceMarketOptionsRequest
  ( LaunchTemplateInstanceMarketOptionsRequest (..),

    -- * Smart constructor
    mkLaunchTemplateInstanceMarketOptionsRequest,

    -- * Lenses
    ltimorMarketType,
    ltimorSpotOptions,
  )
where

import qualified Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptionsRequest as Types
import qualified Network.AWS.EC2.Types.MarketType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The market (purchasing) option for the instances.
--
-- /See:/ 'mkLaunchTemplateInstanceMarketOptionsRequest' smart constructor.
data LaunchTemplateInstanceMarketOptionsRequest = LaunchTemplateInstanceMarketOptionsRequest'
  { -- | The market type.
    marketType :: Core.Maybe Types.MarketType,
    -- | The options for Spot Instances.
    spotOptions :: Core.Maybe Types.LaunchTemplateSpotMarketOptionsRequest
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'LaunchTemplateInstanceMarketOptionsRequest' value with any optional fields omitted.
mkLaunchTemplateInstanceMarketOptionsRequest ::
  LaunchTemplateInstanceMarketOptionsRequest
mkLaunchTemplateInstanceMarketOptionsRequest =
  LaunchTemplateInstanceMarketOptionsRequest'
    { marketType =
        Core.Nothing,
      spotOptions = Core.Nothing
    }

-- | The market type.
--
-- /Note:/ Consider using 'marketType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimorMarketType :: Lens.Lens' LaunchTemplateInstanceMarketOptionsRequest (Core.Maybe Types.MarketType)
ltimorMarketType = Lens.field @"marketType"
{-# DEPRECATED ltimorMarketType "Use generic-lens or generic-optics with 'marketType' instead." #-}

-- | The options for Spot Instances.
--
-- /Note:/ Consider using 'spotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimorSpotOptions :: Lens.Lens' LaunchTemplateInstanceMarketOptionsRequest (Core.Maybe Types.LaunchTemplateSpotMarketOptionsRequest)
ltimorSpotOptions = Lens.field @"spotOptions"
{-# DEPRECATED ltimorSpotOptions "Use generic-lens or generic-optics with 'spotOptions' instead." #-}
