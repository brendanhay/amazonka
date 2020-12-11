-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProductViewSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProductViewSummary
  ( ProductViewSummary (..),

    -- * Smart constructor
    mkProductViewSummary,

    -- * Lenses
    pvsOwner,
    pvsSupportURL,
    pvsShortDescription,
    pvsHasDefaultPath,
    pvsDistributor,
    pvsName,
    pvsId,
    pvsType,
    pvsSupportEmail,
    pvsProductId,
    pvsSupportDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ProductType

-- | Summary information about a product view.
--
-- /See:/ 'mkProductViewSummary' smart constructor.
data ProductViewSummary = ProductViewSummary'
  { owner ::
      Lude.Maybe Lude.Text,
    supportURL :: Lude.Maybe Lude.Text,
    shortDescription :: Lude.Maybe Lude.Text,
    hasDefaultPath :: Lude.Maybe Lude.Bool,
    distributor :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe ProductType,
    supportEmail :: Lude.Maybe Lude.Text,
    productId :: Lude.Maybe Lude.Text,
    supportDescription :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProductViewSummary' with the minimum fields required to make a request.
--
-- * 'distributor' - The distributor of the product. Contact the product administrator for the significance of this value.
-- * 'hasDefaultPath' - Indicates whether the product has a default path. If the product does not have a default path, call 'ListLaunchPaths' to disambiguate between paths. Otherwise, 'ListLaunchPaths' is not required, and the output of 'ProductViewSummary' can be used directly with 'DescribeProvisioningParameters' .
-- * 'id' - The product view identifier.
-- * 'name' - The name of the product.
-- * 'owner' - The owner of the product. Contact the product administrator for the significance of this value.
-- * 'productId' - The product identifier.
-- * 'shortDescription' - Short description of the product.
-- * 'supportDescription' - The description of the support for this Product.
-- * 'supportEmail' - The email contact information to obtain support for this Product.
-- * 'supportURL' - The URL information to obtain support for this Product.
-- * 'type'' - The product type. Contact the product administrator for the significance of this value. If this value is @MARKETPLACE@ , the product was created by AWS Marketplace.
mkProductViewSummary ::
  ProductViewSummary
mkProductViewSummary =
  ProductViewSummary'
    { owner = Lude.Nothing,
      supportURL = Lude.Nothing,
      shortDescription = Lude.Nothing,
      hasDefaultPath = Lude.Nothing,
      distributor = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing,
      supportEmail = Lude.Nothing,
      productId = Lude.Nothing,
      supportDescription = Lude.Nothing
    }

-- | The owner of the product. Contact the product administrator for the significance of this value.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsOwner :: Lens.Lens' ProductViewSummary (Lude.Maybe Lude.Text)
pvsOwner = Lens.lens (owner :: ProductViewSummary -> Lude.Maybe Lude.Text) (\s a -> s {owner = a} :: ProductViewSummary)
{-# DEPRECATED pvsOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The URL information to obtain support for this Product.
--
-- /Note:/ Consider using 'supportURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsSupportURL :: Lens.Lens' ProductViewSummary (Lude.Maybe Lude.Text)
pvsSupportURL = Lens.lens (supportURL :: ProductViewSummary -> Lude.Maybe Lude.Text) (\s a -> s {supportURL = a} :: ProductViewSummary)
{-# DEPRECATED pvsSupportURL "Use generic-lens or generic-optics with 'supportURL' instead." #-}

-- | Short description of the product.
--
-- /Note:/ Consider using 'shortDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsShortDescription :: Lens.Lens' ProductViewSummary (Lude.Maybe Lude.Text)
pvsShortDescription = Lens.lens (shortDescription :: ProductViewSummary -> Lude.Maybe Lude.Text) (\s a -> s {shortDescription = a} :: ProductViewSummary)
{-# DEPRECATED pvsShortDescription "Use generic-lens or generic-optics with 'shortDescription' instead." #-}

-- | Indicates whether the product has a default path. If the product does not have a default path, call 'ListLaunchPaths' to disambiguate between paths. Otherwise, 'ListLaunchPaths' is not required, and the output of 'ProductViewSummary' can be used directly with 'DescribeProvisioningParameters' .
--
-- /Note:/ Consider using 'hasDefaultPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsHasDefaultPath :: Lens.Lens' ProductViewSummary (Lude.Maybe Lude.Bool)
pvsHasDefaultPath = Lens.lens (hasDefaultPath :: ProductViewSummary -> Lude.Maybe Lude.Bool) (\s a -> s {hasDefaultPath = a} :: ProductViewSummary)
{-# DEPRECATED pvsHasDefaultPath "Use generic-lens or generic-optics with 'hasDefaultPath' instead." #-}

-- | The distributor of the product. Contact the product administrator for the significance of this value.
--
-- /Note:/ Consider using 'distributor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsDistributor :: Lens.Lens' ProductViewSummary (Lude.Maybe Lude.Text)
pvsDistributor = Lens.lens (distributor :: ProductViewSummary -> Lude.Maybe Lude.Text) (\s a -> s {distributor = a} :: ProductViewSummary)
{-# DEPRECATED pvsDistributor "Use generic-lens or generic-optics with 'distributor' instead." #-}

-- | The name of the product.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsName :: Lens.Lens' ProductViewSummary (Lude.Maybe Lude.Text)
pvsName = Lens.lens (name :: ProductViewSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ProductViewSummary)
{-# DEPRECATED pvsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The product view identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsId :: Lens.Lens' ProductViewSummary (Lude.Maybe Lude.Text)
pvsId = Lens.lens (id :: ProductViewSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ProductViewSummary)
{-# DEPRECATED pvsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The product type. Contact the product administrator for the significance of this value. If this value is @MARKETPLACE@ , the product was created by AWS Marketplace.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsType :: Lens.Lens' ProductViewSummary (Lude.Maybe ProductType)
pvsType = Lens.lens (type' :: ProductViewSummary -> Lude.Maybe ProductType) (\s a -> s {type' = a} :: ProductViewSummary)
{-# DEPRECATED pvsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The email contact information to obtain support for this Product.
--
-- /Note:/ Consider using 'supportEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsSupportEmail :: Lens.Lens' ProductViewSummary (Lude.Maybe Lude.Text)
pvsSupportEmail = Lens.lens (supportEmail :: ProductViewSummary -> Lude.Maybe Lude.Text) (\s a -> s {supportEmail = a} :: ProductViewSummary)
{-# DEPRECATED pvsSupportEmail "Use generic-lens or generic-optics with 'supportEmail' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsProductId :: Lens.Lens' ProductViewSummary (Lude.Maybe Lude.Text)
pvsProductId = Lens.lens (productId :: ProductViewSummary -> Lude.Maybe Lude.Text) (\s a -> s {productId = a} :: ProductViewSummary)
{-# DEPRECATED pvsProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | The description of the support for this Product.
--
-- /Note:/ Consider using 'supportDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsSupportDescription :: Lens.Lens' ProductViewSummary (Lude.Maybe Lude.Text)
pvsSupportDescription = Lens.lens (supportDescription :: ProductViewSummary -> Lude.Maybe Lude.Text) (\s a -> s {supportDescription = a} :: ProductViewSummary)
{-# DEPRECATED pvsSupportDescription "Use generic-lens or generic-optics with 'supportDescription' instead." #-}

instance Lude.FromJSON ProductViewSummary where
  parseJSON =
    Lude.withObject
      "ProductViewSummary"
      ( \x ->
          ProductViewSummary'
            Lude.<$> (x Lude..:? "Owner")
            Lude.<*> (x Lude..:? "SupportUrl")
            Lude.<*> (x Lude..:? "ShortDescription")
            Lude.<*> (x Lude..:? "HasDefaultPath")
            Lude.<*> (x Lude..:? "Distributor")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "SupportEmail")
            Lude.<*> (x Lude..:? "ProductId")
            Lude.<*> (x Lude..:? "SupportDescription")
      )
