{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceMarketOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceMarketOptionsRequest
  ( InstanceMarketOptionsRequest (..)
  -- * Smart constructor
  , mkInstanceMarketOptionsRequest
  -- * Lenses
  , imorMarketType
  , imorSpotOptions
  ) where

import qualified Network.AWS.EC2.Types.MarketType as Types
import qualified Network.AWS.EC2.Types.SpotMarketOptions as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the market (purchasing) option for the instances.
--
-- /See:/ 'mkInstanceMarketOptionsRequest' smart constructor.
data InstanceMarketOptionsRequest = InstanceMarketOptionsRequest'
  { marketType :: Core.Maybe Types.MarketType
    -- ^ The market type.
  , spotOptions :: Core.Maybe Types.SpotMarketOptions
    -- ^ The options for Spot Instances.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceMarketOptionsRequest' value with any optional fields omitted.
mkInstanceMarketOptionsRequest
    :: InstanceMarketOptionsRequest
mkInstanceMarketOptionsRequest
  = InstanceMarketOptionsRequest'{marketType = Core.Nothing,
                                  spotOptions = Core.Nothing}

-- | The market type.
--
-- /Note:/ Consider using 'marketType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imorMarketType :: Lens.Lens' InstanceMarketOptionsRequest (Core.Maybe Types.MarketType)
imorMarketType = Lens.field @"marketType"
{-# INLINEABLE imorMarketType #-}
{-# DEPRECATED marketType "Use generic-lens or generic-optics with 'marketType' instead"  #-}

-- | The options for Spot Instances.
--
-- /Note:/ Consider using 'spotOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imorSpotOptions :: Lens.Lens' InstanceMarketOptionsRequest (Core.Maybe Types.SpotMarketOptions)
imorSpotOptions = Lens.field @"spotOptions"
{-# INLINEABLE imorSpotOptions #-}
{-# DEPRECATED spotOptions "Use generic-lens or generic-optics with 'spotOptions' instead"  #-}

instance Core.ToQuery InstanceMarketOptionsRequest where
        toQuery InstanceMarketOptionsRequest{..}
          = Core.maybe Core.mempty (Core.toQueryPair "MarketType") marketType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SpotOptions") spotOptions
