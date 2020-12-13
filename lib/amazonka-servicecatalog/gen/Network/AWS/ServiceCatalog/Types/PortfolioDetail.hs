{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.PortfolioDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.PortfolioDetail
  ( PortfolioDetail (..),

    -- * Smart constructor
    mkPortfolioDetail,

    -- * Lenses
    pdARN,
    pdCreatedTime,
    pdId,
    pdDisplayName,
    pdDescription,
    pdProviderName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a portfolio.
--
-- /See:/ 'mkPortfolioDetail' smart constructor.
data PortfolioDetail = PortfolioDetail'
  { -- | The ARN assigned to the portfolio.
    arn :: Lude.Maybe Lude.Text,
    -- | The UTC time stamp of the creation time.
    createdTime :: Lude.Maybe Lude.Timestamp,
    -- | The portfolio identifier.
    id :: Lude.Maybe Lude.Text,
    -- | The name to use for display purposes.
    displayName :: Lude.Maybe Lude.Text,
    -- | The description of the portfolio.
    description :: Lude.Maybe Lude.Text,
    -- | The name of the portfolio provider.
    providerName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PortfolioDetail' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN assigned to the portfolio.
-- * 'createdTime' - The UTC time stamp of the creation time.
-- * 'id' - The portfolio identifier.
-- * 'displayName' - The name to use for display purposes.
-- * 'description' - The description of the portfolio.
-- * 'providerName' - The name of the portfolio provider.
mkPortfolioDetail ::
  PortfolioDetail
mkPortfolioDetail =
  PortfolioDetail'
    { arn = Lude.Nothing,
      createdTime = Lude.Nothing,
      id = Lude.Nothing,
      displayName = Lude.Nothing,
      description = Lude.Nothing,
      providerName = Lude.Nothing
    }

-- | The ARN assigned to the portfolio.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdARN :: Lens.Lens' PortfolioDetail (Lude.Maybe Lude.Text)
pdARN = Lens.lens (arn :: PortfolioDetail -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: PortfolioDetail)
{-# DEPRECATED pdARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The UTC time stamp of the creation time.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdCreatedTime :: Lens.Lens' PortfolioDetail (Lude.Maybe Lude.Timestamp)
pdCreatedTime = Lens.lens (createdTime :: PortfolioDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: PortfolioDetail)
{-# DEPRECATED pdCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The portfolio identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdId :: Lens.Lens' PortfolioDetail (Lude.Maybe Lude.Text)
pdId = Lens.lens (id :: PortfolioDetail -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: PortfolioDetail)
{-# DEPRECATED pdId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name to use for display purposes.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDisplayName :: Lens.Lens' PortfolioDetail (Lude.Maybe Lude.Text)
pdDisplayName = Lens.lens (displayName :: PortfolioDetail -> Lude.Maybe Lude.Text) (\s a -> s {displayName = a} :: PortfolioDetail)
{-# DEPRECATED pdDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The description of the portfolio.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdDescription :: Lens.Lens' PortfolioDetail (Lude.Maybe Lude.Text)
pdDescription = Lens.lens (description :: PortfolioDetail -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PortfolioDetail)
{-# DEPRECATED pdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the portfolio provider.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdProviderName :: Lens.Lens' PortfolioDetail (Lude.Maybe Lude.Text)
pdProviderName = Lens.lens (providerName :: PortfolioDetail -> Lude.Maybe Lude.Text) (\s a -> s {providerName = a} :: PortfolioDetail)
{-# DEPRECATED pdProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

instance Lude.FromJSON PortfolioDetail where
  parseJSON =
    Lude.withObject
      "PortfolioDetail"
      ( \x ->
          PortfolioDetail'
            Lude.<$> (x Lude..:? "ARN")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "DisplayName")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "ProviderName")
      )
