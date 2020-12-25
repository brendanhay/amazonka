{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    pvsDistributor,
    pvsHasDefaultPath,
    pvsId,
    pvsName,
    pvsOwner,
    pvsProductId,
    pvsShortDescription,
    pvsSupportDescription,
    pvsSupportEmail,
    pvsSupportUrl,
    pvsType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.Distributor as Types
import qualified Network.AWS.ServiceCatalog.Types.Id as Types
import qualified Network.AWS.ServiceCatalog.Types.Name as Types
import qualified Network.AWS.ServiceCatalog.Types.ProductId as Types
import qualified Network.AWS.ServiceCatalog.Types.ProductType as Types
import qualified Network.AWS.ServiceCatalog.Types.ProductViewOwner as Types
import qualified Network.AWS.ServiceCatalog.Types.ShortDescription as Types
import qualified Network.AWS.ServiceCatalog.Types.SupportDescription as Types
import qualified Network.AWS.ServiceCatalog.Types.SupportEmail as Types
import qualified Network.AWS.ServiceCatalog.Types.SupportUrl as Types

-- | Summary information about a product view.
--
-- /See:/ 'mkProductViewSummary' smart constructor.
data ProductViewSummary = ProductViewSummary'
  { -- | The distributor of the product. Contact the product administrator for the significance of this value.
    distributor :: Core.Maybe Types.Distributor,
    -- | Indicates whether the product has a default path. If the product does not have a default path, call 'ListLaunchPaths' to disambiguate between paths. Otherwise, 'ListLaunchPaths' is not required, and the output of 'ProductViewSummary' can be used directly with 'DescribeProvisioningParameters' .
    hasDefaultPath :: Core.Maybe Core.Bool,
    -- | The product view identifier.
    id :: Core.Maybe Types.Id,
    -- | The name of the product.
    name :: Core.Maybe Types.Name,
    -- | The owner of the product. Contact the product administrator for the significance of this value.
    owner :: Core.Maybe Types.ProductViewOwner,
    -- | The product identifier.
    productId :: Core.Maybe Types.ProductId,
    -- | Short description of the product.
    shortDescription :: Core.Maybe Types.ShortDescription,
    -- | The description of the support for this Product.
    supportDescription :: Core.Maybe Types.SupportDescription,
    -- | The email contact information to obtain support for this Product.
    supportEmail :: Core.Maybe Types.SupportEmail,
    -- | The URL information to obtain support for this Product.
    supportUrl :: Core.Maybe Types.SupportUrl,
    -- | The product type. Contact the product administrator for the significance of this value. If this value is @MARKETPLACE@ , the product was created by AWS Marketplace.
    type' :: Core.Maybe Types.ProductType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProductViewSummary' value with any optional fields omitted.
mkProductViewSummary ::
  ProductViewSummary
mkProductViewSummary =
  ProductViewSummary'
    { distributor = Core.Nothing,
      hasDefaultPath = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing,
      owner = Core.Nothing,
      productId = Core.Nothing,
      shortDescription = Core.Nothing,
      supportDescription = Core.Nothing,
      supportEmail = Core.Nothing,
      supportUrl = Core.Nothing,
      type' = Core.Nothing
    }

-- | The distributor of the product. Contact the product administrator for the significance of this value.
--
-- /Note:/ Consider using 'distributor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsDistributor :: Lens.Lens' ProductViewSummary (Core.Maybe Types.Distributor)
pvsDistributor = Lens.field @"distributor"
{-# DEPRECATED pvsDistributor "Use generic-lens or generic-optics with 'distributor' instead." #-}

-- | Indicates whether the product has a default path. If the product does not have a default path, call 'ListLaunchPaths' to disambiguate between paths. Otherwise, 'ListLaunchPaths' is not required, and the output of 'ProductViewSummary' can be used directly with 'DescribeProvisioningParameters' .
--
-- /Note:/ Consider using 'hasDefaultPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsHasDefaultPath :: Lens.Lens' ProductViewSummary (Core.Maybe Core.Bool)
pvsHasDefaultPath = Lens.field @"hasDefaultPath"
{-# DEPRECATED pvsHasDefaultPath "Use generic-lens or generic-optics with 'hasDefaultPath' instead." #-}

-- | The product view identifier.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsId :: Lens.Lens' ProductViewSummary (Core.Maybe Types.Id)
pvsId = Lens.field @"id"
{-# DEPRECATED pvsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the product.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsName :: Lens.Lens' ProductViewSummary (Core.Maybe Types.Name)
pvsName = Lens.field @"name"
{-# DEPRECATED pvsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The owner of the product. Contact the product administrator for the significance of this value.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsOwner :: Lens.Lens' ProductViewSummary (Core.Maybe Types.ProductViewOwner)
pvsOwner = Lens.field @"owner"
{-# DEPRECATED pvsOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The product identifier.
--
-- /Note:/ Consider using 'productId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsProductId :: Lens.Lens' ProductViewSummary (Core.Maybe Types.ProductId)
pvsProductId = Lens.field @"productId"
{-# DEPRECATED pvsProductId "Use generic-lens or generic-optics with 'productId' instead." #-}

-- | Short description of the product.
--
-- /Note:/ Consider using 'shortDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsShortDescription :: Lens.Lens' ProductViewSummary (Core.Maybe Types.ShortDescription)
pvsShortDescription = Lens.field @"shortDescription"
{-# DEPRECATED pvsShortDescription "Use generic-lens or generic-optics with 'shortDescription' instead." #-}

-- | The description of the support for this Product.
--
-- /Note:/ Consider using 'supportDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsSupportDescription :: Lens.Lens' ProductViewSummary (Core.Maybe Types.SupportDescription)
pvsSupportDescription = Lens.field @"supportDescription"
{-# DEPRECATED pvsSupportDescription "Use generic-lens or generic-optics with 'supportDescription' instead." #-}

-- | The email contact information to obtain support for this Product.
--
-- /Note:/ Consider using 'supportEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsSupportEmail :: Lens.Lens' ProductViewSummary (Core.Maybe Types.SupportEmail)
pvsSupportEmail = Lens.field @"supportEmail"
{-# DEPRECATED pvsSupportEmail "Use generic-lens or generic-optics with 'supportEmail' instead." #-}

-- | The URL information to obtain support for this Product.
--
-- /Note:/ Consider using 'supportUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsSupportUrl :: Lens.Lens' ProductViewSummary (Core.Maybe Types.SupportUrl)
pvsSupportUrl = Lens.field @"supportUrl"
{-# DEPRECATED pvsSupportUrl "Use generic-lens or generic-optics with 'supportUrl' instead." #-}

-- | The product type. Contact the product administrator for the significance of this value. If this value is @MARKETPLACE@ , the product was created by AWS Marketplace.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvsType :: Lens.Lens' ProductViewSummary (Core.Maybe Types.ProductType)
pvsType = Lens.field @"type'"
{-# DEPRECATED pvsType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON ProductViewSummary where
  parseJSON =
    Core.withObject "ProductViewSummary" Core.$
      \x ->
        ProductViewSummary'
          Core.<$> (x Core..:? "Distributor")
          Core.<*> (x Core..:? "HasDefaultPath")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "Owner")
          Core.<*> (x Core..:? "ProductId")
          Core.<*> (x Core..:? "ShortDescription")
          Core.<*> (x Core..:? "SupportDescription")
          Core.<*> (x Core..:? "SupportEmail")
          Core.<*> (x Core..:? "SupportUrl")
          Core.<*> (x Core..:? "Type")
