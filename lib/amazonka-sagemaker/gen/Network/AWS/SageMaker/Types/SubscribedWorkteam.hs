-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SubscribedWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SubscribedWorkteam
  ( SubscribedWorkteam (..),

    -- * Smart constructor
    mkSubscribedWorkteam,

    -- * Lenses
    swMarketplaceTitle,
    swSellerName,
    swListingId,
    swMarketplaceDescription,
    swWorkteamARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a work team of a vendor that does the a labelling job.
--
-- /See:/ 'mkSubscribedWorkteam' smart constructor.
data SubscribedWorkteam = SubscribedWorkteam'
  { marketplaceTitle ::
      Lude.Maybe Lude.Text,
    sellerName :: Lude.Maybe Lude.Text,
    listingId :: Lude.Maybe Lude.Text,
    marketplaceDescription :: Lude.Maybe Lude.Text,
    workteamARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubscribedWorkteam' with the minimum fields required to make a request.
--
-- * 'listingId' - Marketplace product listing ID.
-- * 'marketplaceDescription' - The description of the vendor from the Amazon Marketplace.
-- * 'marketplaceTitle' - The title of the service provided by the vendor in the Amazon Marketplace.
-- * 'sellerName' - The name of the vendor in the Amazon Marketplace.
-- * 'workteamARN' - The Amazon Resource Name (ARN) of the vendor that you have subscribed.
mkSubscribedWorkteam ::
  -- | 'workteamARN'
  Lude.Text ->
  SubscribedWorkteam
mkSubscribedWorkteam pWorkteamARN_ =
  SubscribedWorkteam'
    { marketplaceTitle = Lude.Nothing,
      sellerName = Lude.Nothing,
      listingId = Lude.Nothing,
      marketplaceDescription = Lude.Nothing,
      workteamARN = pWorkteamARN_
    }

-- | The title of the service provided by the vendor in the Amazon Marketplace.
--
-- /Note:/ Consider using 'marketplaceTitle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swMarketplaceTitle :: Lens.Lens' SubscribedWorkteam (Lude.Maybe Lude.Text)
swMarketplaceTitle = Lens.lens (marketplaceTitle :: SubscribedWorkteam -> Lude.Maybe Lude.Text) (\s a -> s {marketplaceTitle = a} :: SubscribedWorkteam)
{-# DEPRECATED swMarketplaceTitle "Use generic-lens or generic-optics with 'marketplaceTitle' instead." #-}

-- | The name of the vendor in the Amazon Marketplace.
--
-- /Note:/ Consider using 'sellerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swSellerName :: Lens.Lens' SubscribedWorkteam (Lude.Maybe Lude.Text)
swSellerName = Lens.lens (sellerName :: SubscribedWorkteam -> Lude.Maybe Lude.Text) (\s a -> s {sellerName = a} :: SubscribedWorkteam)
{-# DEPRECATED swSellerName "Use generic-lens or generic-optics with 'sellerName' instead." #-}

-- | Marketplace product listing ID.
--
-- /Note:/ Consider using 'listingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swListingId :: Lens.Lens' SubscribedWorkteam (Lude.Maybe Lude.Text)
swListingId = Lens.lens (listingId :: SubscribedWorkteam -> Lude.Maybe Lude.Text) (\s a -> s {listingId = a} :: SubscribedWorkteam)
{-# DEPRECATED swListingId "Use generic-lens or generic-optics with 'listingId' instead." #-}

-- | The description of the vendor from the Amazon Marketplace.
--
-- /Note:/ Consider using 'marketplaceDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swMarketplaceDescription :: Lens.Lens' SubscribedWorkteam (Lude.Maybe Lude.Text)
swMarketplaceDescription = Lens.lens (marketplaceDescription :: SubscribedWorkteam -> Lude.Maybe Lude.Text) (\s a -> s {marketplaceDescription = a} :: SubscribedWorkteam)
{-# DEPRECATED swMarketplaceDescription "Use generic-lens or generic-optics with 'marketplaceDescription' instead." #-}

-- | The Amazon Resource Name (ARN) of the vendor that you have subscribed.
--
-- /Note:/ Consider using 'workteamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
swWorkteamARN :: Lens.Lens' SubscribedWorkteam Lude.Text
swWorkteamARN = Lens.lens (workteamARN :: SubscribedWorkteam -> Lude.Text) (\s a -> s {workteamARN = a} :: SubscribedWorkteam)
{-# DEPRECATED swWorkteamARN "Use generic-lens or generic-optics with 'workteamARN' instead." #-}

instance Lude.FromJSON SubscribedWorkteam where
  parseJSON =
    Lude.withObject
      "SubscribedWorkteam"
      ( \x ->
          SubscribedWorkteam'
            Lude.<$> (x Lude..:? "MarketplaceTitle")
            Lude.<*> (x Lude..:? "SellerName")
            Lude.<*> (x Lude..:? "ListingId")
            Lude.<*> (x Lude..:? "MarketplaceDescription")
            Lude.<*> (x Lude..: "WorkteamArn")
      )
