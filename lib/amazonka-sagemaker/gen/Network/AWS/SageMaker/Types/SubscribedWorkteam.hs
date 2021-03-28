{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SubscribedWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.SubscribedWorkteam
  ( SubscribedWorkteam (..)
  -- * Smart constructor
  , mkSubscribedWorkteam
  -- * Lenses
  , swWorkteamArn
  , swListingId
  , swMarketplaceDescription
  , swMarketplaceTitle
  , swSellerName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.String200 as Types
import qualified Network.AWS.SageMaker.Types.WorkteamArn as Types

-- | Describes a work team of a vendor that does the a labelling job.
--
-- /See:/ 'mkSubscribedWorkteam' smart constructor.
data SubscribedWorkteam = SubscribedWorkteam'
  { workteamArn :: Types.WorkteamArn
    -- ^ The Amazon Resource Name (ARN) of the vendor that you have subscribed.
  , listingId :: Core.Maybe Core.Text
    -- ^ Marketplace product listing ID.
  , marketplaceDescription :: Core.Maybe Types.String200
    -- ^ The description of the vendor from the Amazon Marketplace.
  , marketplaceTitle :: Core.Maybe Types.String200
    -- ^ The title of the service provided by the vendor in the Amazon Marketplace.
  , sellerName :: Core.Maybe Core.Text
    -- ^ The name of the vendor in the Amazon Marketplace.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubscribedWorkteam' value with any optional fields omitted.
mkSubscribedWorkteam
    :: Types.WorkteamArn -- ^ 'workteamArn'
    -> SubscribedWorkteam
mkSubscribedWorkteam workteamArn
  = SubscribedWorkteam'{workteamArn, listingId = Core.Nothing,
                        marketplaceDescription = Core.Nothing,
                        marketplaceTitle = Core.Nothing, sellerName = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the vendor that you have subscribed.
--
-- /Note:/ Consider using 'workteamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swWorkteamArn :: Lens.Lens' SubscribedWorkteam Types.WorkteamArn
swWorkteamArn = Lens.field @"workteamArn"
{-# INLINEABLE swWorkteamArn #-}
{-# DEPRECATED workteamArn "Use generic-lens or generic-optics with 'workteamArn' instead"  #-}

-- | Marketplace product listing ID.
--
-- /Note:/ Consider using 'listingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swListingId :: Lens.Lens' SubscribedWorkteam (Core.Maybe Core.Text)
swListingId = Lens.field @"listingId"
{-# INLINEABLE swListingId #-}
{-# DEPRECATED listingId "Use generic-lens or generic-optics with 'listingId' instead"  #-}

-- | The description of the vendor from the Amazon Marketplace.
--
-- /Note:/ Consider using 'marketplaceDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swMarketplaceDescription :: Lens.Lens' SubscribedWorkteam (Core.Maybe Types.String200)
swMarketplaceDescription = Lens.field @"marketplaceDescription"
{-# INLINEABLE swMarketplaceDescription #-}
{-# DEPRECATED marketplaceDescription "Use generic-lens or generic-optics with 'marketplaceDescription' instead"  #-}

-- | The title of the service provided by the vendor in the Amazon Marketplace.
--
-- /Note:/ Consider using 'marketplaceTitle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swMarketplaceTitle :: Lens.Lens' SubscribedWorkteam (Core.Maybe Types.String200)
swMarketplaceTitle = Lens.field @"marketplaceTitle"
{-# INLINEABLE swMarketplaceTitle #-}
{-# DEPRECATED marketplaceTitle "Use generic-lens or generic-optics with 'marketplaceTitle' instead"  #-}

-- | The name of the vendor in the Amazon Marketplace.
--
-- /Note:/ Consider using 'sellerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swSellerName :: Lens.Lens' SubscribedWorkteam (Core.Maybe Core.Text)
swSellerName = Lens.field @"sellerName"
{-# INLINEABLE swSellerName #-}
{-# DEPRECATED sellerName "Use generic-lens or generic-optics with 'sellerName' instead"  #-}

instance Core.FromJSON SubscribedWorkteam where
        parseJSON
          = Core.withObject "SubscribedWorkteam" Core.$
              \ x ->
                SubscribedWorkteam' Core.<$>
                  (x Core..: "WorkteamArn") Core.<*> x Core..:? "ListingId" Core.<*>
                    x Core..:? "MarketplaceDescription"
                    Core.<*> x Core..:? "MarketplaceTitle"
                    Core.<*> x Core..:? "SellerName"
