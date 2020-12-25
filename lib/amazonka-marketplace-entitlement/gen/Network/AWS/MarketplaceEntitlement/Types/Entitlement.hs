{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceEntitlement.Types.Entitlement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceEntitlement.Types.Entitlement
  ( Entitlement (..),

    -- * Smart constructor
    mkEntitlement,

    -- * Lenses
    eCustomerIdentifier,
    eDimension,
    eExpirationDate,
    eProductCode,
    eValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MarketplaceEntitlement.Types.EntitlementValue as Types
import qualified Network.AWS.MarketplaceEntitlement.Types.NonEmptyString as Types
import qualified Network.AWS.MarketplaceEntitlement.Types.ProductCode as Types
import qualified Network.AWS.Prelude as Core

-- | An entitlement represents capacity in a product owned by the customer. For example, a customer might own some number of users or seats in an SaaS application or some amount of data capacity in a multi-tenant database.
--
-- /See:/ 'mkEntitlement' smart constructor.
data Entitlement = Entitlement'
  { -- | The customer identifier is a handle to each unique customer in an application. Customer identifiers are obtained through the ResolveCustomer operation in AWS Marketplace Metering Service.
    customerIdentifier :: Core.Maybe Types.NonEmptyString,
    -- | The dimension for which the given entitlement applies. Dimensions represent categories of capacity in a product and are specified when the product is listed in AWS Marketplace.
    dimension :: Core.Maybe Types.NonEmptyString,
    -- | The expiration date represents the minimum date through which this entitlement is expected to remain valid. For contractual products listed on AWS Marketplace, the expiration date is the date at which the customer will renew or cancel their contract. Customers who are opting to renew their contract will still have entitlements with an expiration date.
    expirationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The product code for which the given entitlement applies. Product codes are provided by AWS Marketplace when the product listing is created.
    productCode :: Core.Maybe Types.ProductCode,
    -- | The EntitlementValue represents the amount of capacity that the customer is entitled to for the product.
    value :: Core.Maybe Types.EntitlementValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Entitlement' value with any optional fields omitted.
mkEntitlement ::
  Entitlement
mkEntitlement =
  Entitlement'
    { customerIdentifier = Core.Nothing,
      dimension = Core.Nothing,
      expirationDate = Core.Nothing,
      productCode = Core.Nothing,
      value = Core.Nothing
    }

-- | The customer identifier is a handle to each unique customer in an application. Customer identifiers are obtained through the ResolveCustomer operation in AWS Marketplace Metering Service.
--
-- /Note:/ Consider using 'customerIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCustomerIdentifier :: Lens.Lens' Entitlement (Core.Maybe Types.NonEmptyString)
eCustomerIdentifier = Lens.field @"customerIdentifier"
{-# DEPRECATED eCustomerIdentifier "Use generic-lens or generic-optics with 'customerIdentifier' instead." #-}

-- | The dimension for which the given entitlement applies. Dimensions represent categories of capacity in a product and are specified when the product is listed in AWS Marketplace.
--
-- /Note:/ Consider using 'dimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDimension :: Lens.Lens' Entitlement (Core.Maybe Types.NonEmptyString)
eDimension = Lens.field @"dimension"
{-# DEPRECATED eDimension "Use generic-lens or generic-optics with 'dimension' instead." #-}

-- | The expiration date represents the minimum date through which this entitlement is expected to remain valid. For contractual products listed on AWS Marketplace, the expiration date is the date at which the customer will renew or cancel their contract. Customers who are opting to renew their contract will still have entitlements with an expiration date.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExpirationDate :: Lens.Lens' Entitlement (Core.Maybe Core.NominalDiffTime)
eExpirationDate = Lens.field @"expirationDate"
{-# DEPRECATED eExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | The product code for which the given entitlement applies. Product codes are provided by AWS Marketplace when the product listing is created.
--
-- /Note:/ Consider using 'productCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eProductCode :: Lens.Lens' Entitlement (Core.Maybe Types.ProductCode)
eProductCode = Lens.field @"productCode"
{-# DEPRECATED eProductCode "Use generic-lens or generic-optics with 'productCode' instead." #-}

-- | The EntitlementValue represents the amount of capacity that the customer is entitled to for the product.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eValue :: Lens.Lens' Entitlement (Core.Maybe Types.EntitlementValue)
eValue = Lens.field @"value"
{-# DEPRECATED eValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Entitlement where
  parseJSON =
    Core.withObject "Entitlement" Core.$
      \x ->
        Entitlement'
          Core.<$> (x Core..:? "CustomerIdentifier")
          Core.<*> (x Core..:? "Dimension")
          Core.<*> (x Core..:? "ExpirationDate")
          Core.<*> (x Core..:? "ProductCode")
          Core.<*> (x Core..:? "Value")
