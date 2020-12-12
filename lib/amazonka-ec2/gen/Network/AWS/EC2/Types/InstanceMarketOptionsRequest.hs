{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceMarketOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceMarketOptionsRequest
  ( InstanceMarketOptionsRequest (..),

    -- * Smart constructor
    mkInstanceMarketOptionsRequest,

    -- * Lenses
    imorMarketType,
    imorSpotOptions,
  )
where

import Network.AWS.EC2.Types.MarketType
import Network.AWS.EC2.Types.SpotMarketOptions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the market (purchasing) option for the instances.
--
-- /See:/ 'mkInstanceMarketOptionsRequest' smart constructor.
data InstanceMarketOptionsRequest = InstanceMarketOptionsRequest'
  { marketType ::
      Lude.Maybe MarketType,
    spotOptions ::
      Lude.Maybe SpotMarketOptions
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceMarketOptionsRequest' with the minimum fields required to make a request.
--
-- * 'marketType' - The market type.
-- * 'spotOptions' - The options for Spot Instances.
mkInstanceMarketOptionsRequest ::
  InstanceMarketOptionsRequest
mkInstanceMarketOptionsRequest =
  InstanceMarketOptionsRequest'
    { marketType = Lude.Nothing,
      spotOptions = Lude.Nothing
    }

-- | The market type.
--
-- /Note:/ Consider using 'marketType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imorMarketType :: Lens.Lens' InstanceMarketOptionsRequest (Lude.Maybe MarketType)
imorMarketType = Lens.lens (marketType :: InstanceMarketOptionsRequest -> Lude.Maybe MarketType) (\s a -> s {marketType = a} :: InstanceMarketOptionsRequest)
{-# DEPRECATED imorMarketType "Use generic-lens or generic-optics with 'marketType' instead." #-}

-- | The options for Spot Instances.
--
-- /Note:/ Consider using 'spotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imorSpotOptions :: Lens.Lens' InstanceMarketOptionsRequest (Lude.Maybe SpotMarketOptions)
imorSpotOptions = Lens.lens (spotOptions :: InstanceMarketOptionsRequest -> Lude.Maybe SpotMarketOptions) (\s a -> s {spotOptions = a} :: InstanceMarketOptionsRequest)
{-# DEPRECATED imorSpotOptions "Use generic-lens or generic-optics with 'spotOptions' instead." #-}

instance Lude.ToQuery InstanceMarketOptionsRequest where
  toQuery InstanceMarketOptionsRequest' {..} =
    Lude.mconcat
      [ "MarketType" Lude.=: marketType,
        "SpotOptions" Lude.=: spotOptions
      ]
