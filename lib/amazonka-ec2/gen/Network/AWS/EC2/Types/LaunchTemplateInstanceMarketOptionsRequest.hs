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

import Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptionsRequest
import Network.AWS.EC2.Types.MarketType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The market (purchasing) option for the instances.
--
-- /See:/ 'mkLaunchTemplateInstanceMarketOptionsRequest' smart constructor.
data LaunchTemplateInstanceMarketOptionsRequest = LaunchTemplateInstanceMarketOptionsRequest'
  { marketType ::
      Lude.Maybe
        MarketType,
    spotOptions ::
      Lude.Maybe
        LaunchTemplateSpotMarketOptionsRequest
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateInstanceMarketOptionsRequest' with the minimum fields required to make a request.
--
-- * 'marketType' - The market type.
-- * 'spotOptions' - The options for Spot Instances.
mkLaunchTemplateInstanceMarketOptionsRequest ::
  LaunchTemplateInstanceMarketOptionsRequest
mkLaunchTemplateInstanceMarketOptionsRequest =
  LaunchTemplateInstanceMarketOptionsRequest'
    { marketType =
        Lude.Nothing,
      spotOptions = Lude.Nothing
    }

-- | The market type.
--
-- /Note:/ Consider using 'marketType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimorMarketType :: Lens.Lens' LaunchTemplateInstanceMarketOptionsRequest (Lude.Maybe MarketType)
ltimorMarketType = Lens.lens (marketType :: LaunchTemplateInstanceMarketOptionsRequest -> Lude.Maybe MarketType) (\s a -> s {marketType = a} :: LaunchTemplateInstanceMarketOptionsRequest)
{-# DEPRECATED ltimorMarketType "Use generic-lens or generic-optics with 'marketType' instead." #-}

-- | The options for Spot Instances.
--
-- /Note:/ Consider using 'spotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltimorSpotOptions :: Lens.Lens' LaunchTemplateInstanceMarketOptionsRequest (Lude.Maybe LaunchTemplateSpotMarketOptionsRequest)
ltimorSpotOptions = Lens.lens (spotOptions :: LaunchTemplateInstanceMarketOptionsRequest -> Lude.Maybe LaunchTemplateSpotMarketOptionsRequest) (\s a -> s {spotOptions = a} :: LaunchTemplateInstanceMarketOptionsRequest)
{-# DEPRECATED ltimorSpotOptions "Use generic-lens or generic-optics with 'spotOptions' instead." #-}

instance Lude.ToQuery LaunchTemplateInstanceMarketOptionsRequest where
  toQuery LaunchTemplateInstanceMarketOptionsRequest' {..} =
    Lude.mconcat
      [ "MarketType" Lude.=: marketType,
        "SpotOptions" Lude.=: spotOptions
      ]
