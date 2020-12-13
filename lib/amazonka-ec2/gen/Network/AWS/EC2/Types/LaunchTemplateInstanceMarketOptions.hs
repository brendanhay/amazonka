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

import Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptions
import Network.AWS.EC2.Types.MarketType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The market (purchasing) option for the instances.
--
-- /See:/ 'mkLaunchTemplateInstanceMarketOptions' smart constructor.
data LaunchTemplateInstanceMarketOptions = LaunchTemplateInstanceMarketOptions'
  { -- | The market type.
    marketType :: Lude.Maybe MarketType,
    -- | The options for Spot Instances.
    spotOptions :: Lude.Maybe LaunchTemplateSpotMarketOptions
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateInstanceMarketOptions' with the minimum fields required to make a request.
--
-- * 'marketType' - The market type.
-- * 'spotOptions' - The options for Spot Instances.
mkLaunchTemplateInstanceMarketOptions ::
  LaunchTemplateInstanceMarketOptions
mkLaunchTemplateInstanceMarketOptions =
  LaunchTemplateInstanceMarketOptions'
    { marketType = Lude.Nothing,
      spotOptions = Lude.Nothing
    }

-- | The market type.
--
-- /Note:/ Consider using 'marketType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimoMarketType :: Lens.Lens' LaunchTemplateInstanceMarketOptions (Lude.Maybe MarketType)
ltimoMarketType = Lens.lens (marketType :: LaunchTemplateInstanceMarketOptions -> Lude.Maybe MarketType) (\s a -> s {marketType = a} :: LaunchTemplateInstanceMarketOptions)
{-# DEPRECATED ltimoMarketType "Use generic-lens or generic-optics with 'marketType' instead." #-}

-- | The options for Spot Instances.
--
-- /Note:/ Consider using 'spotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimoSpotOptions :: Lens.Lens' LaunchTemplateInstanceMarketOptions (Lude.Maybe LaunchTemplateSpotMarketOptions)
ltimoSpotOptions = Lens.lens (spotOptions :: LaunchTemplateInstanceMarketOptions -> Lude.Maybe LaunchTemplateSpotMarketOptions) (\s a -> s {spotOptions = a} :: LaunchTemplateInstanceMarketOptions)
{-# DEPRECATED ltimoSpotOptions "Use generic-lens or generic-optics with 'spotOptions' instead." #-}

instance Lude.FromXML LaunchTemplateInstanceMarketOptions where
  parseXML x =
    LaunchTemplateInstanceMarketOptions'
      Lude.<$> (x Lude..@? "marketType") Lude.<*> (x Lude..@? "spotOptions")
