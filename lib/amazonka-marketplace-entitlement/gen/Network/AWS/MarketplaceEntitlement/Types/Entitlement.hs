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
    eDimension,
    eValue,
    eExpirationDate,
    eCustomerIdentifier,
    eProductCode,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceEntitlement.Types.EntitlementValue
import qualified Network.AWS.Prelude as Lude

-- | An entitlement represents capacity in a product owned by the customer. For example, a customer might own some number of users or seats in an SaaS application or some amount of data capacity in a multi-tenant database.
--
-- /See:/ 'mkEntitlement' smart constructor.
data Entitlement = Entitlement'
  { dimension :: Lude.Maybe Lude.Text,
    value :: Lude.Maybe EntitlementValue,
    expirationDate :: Lude.Maybe Lude.Timestamp,
    customerIdentifier :: Lude.Maybe Lude.Text,
    productCode :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Entitlement' with the minimum fields required to make a request.
--
-- * 'customerIdentifier' - The customer identifier is a handle to each unique customer in an application. Customer identifiers are obtained through the ResolveCustomer operation in AWS Marketplace Metering Service.
-- * 'dimension' - The dimension for which the given entitlement applies. Dimensions represent categories of capacity in a product and are specified when the product is listed in AWS Marketplace.
-- * 'expirationDate' - The expiration date represents the minimum date through which this entitlement is expected to remain valid. For contractual products listed on AWS Marketplace, the expiration date is the date at which the customer will renew or cancel their contract. Customers who are opting to renew their contract will still have entitlements with an expiration date.
-- * 'productCode' - The product code for which the given entitlement applies. Product codes are provided by AWS Marketplace when the product listing is created.
-- * 'value' - The EntitlementValue represents the amount of capacity that the customer is entitled to for the product.
mkEntitlement ::
  Entitlement
mkEntitlement =
  Entitlement'
    { dimension = Lude.Nothing,
      value = Lude.Nothing,
      expirationDate = Lude.Nothing,
      customerIdentifier = Lude.Nothing,
      productCode = Lude.Nothing
    }

-- | The dimension for which the given entitlement applies. Dimensions represent categories of capacity in a product and are specified when the product is listed in AWS Marketplace.
--
-- /Note:/ Consider using 'dimension' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDimension :: Lens.Lens' Entitlement (Lude.Maybe Lude.Text)
eDimension = Lens.lens (dimension :: Entitlement -> Lude.Maybe Lude.Text) (\s a -> s {dimension = a} :: Entitlement)
{-# DEPRECATED eDimension "Use generic-lens or generic-optics with 'dimension' instead." #-}

-- | The EntitlementValue represents the amount of capacity that the customer is entitled to for the product.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eValue :: Lens.Lens' Entitlement (Lude.Maybe EntitlementValue)
eValue = Lens.lens (value :: Entitlement -> Lude.Maybe EntitlementValue) (\s a -> s {value = a} :: Entitlement)
{-# DEPRECATED eValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The expiration date represents the minimum date through which this entitlement is expected to remain valid. For contractual products listed on AWS Marketplace, the expiration date is the date at which the customer will renew or cancel their contract. Customers who are opting to renew their contract will still have entitlements with an expiration date.
--
-- /Note:/ Consider using 'expirationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExpirationDate :: Lens.Lens' Entitlement (Lude.Maybe Lude.Timestamp)
eExpirationDate = Lens.lens (expirationDate :: Entitlement -> Lude.Maybe Lude.Timestamp) (\s a -> s {expirationDate = a} :: Entitlement)
{-# DEPRECATED eExpirationDate "Use generic-lens or generic-optics with 'expirationDate' instead." #-}

-- | The customer identifier is a handle to each unique customer in an application. Customer identifiers are obtained through the ResolveCustomer operation in AWS Marketplace Metering Service.
--
-- /Note:/ Consider using 'customerIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCustomerIdentifier :: Lens.Lens' Entitlement (Lude.Maybe Lude.Text)
eCustomerIdentifier = Lens.lens (customerIdentifier :: Entitlement -> Lude.Maybe Lude.Text) (\s a -> s {customerIdentifier = a} :: Entitlement)
{-# DEPRECATED eCustomerIdentifier "Use generic-lens or generic-optics with 'customerIdentifier' instead." #-}

-- | The product code for which the given entitlement applies. Product codes are provided by AWS Marketplace when the product listing is created.
--
-- /Note:/ Consider using 'productCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eProductCode :: Lens.Lens' Entitlement (Lude.Maybe Lude.Text)
eProductCode = Lens.lens (productCode :: Entitlement -> Lude.Maybe Lude.Text) (\s a -> s {productCode = a} :: Entitlement)
{-# DEPRECATED eProductCode "Use generic-lens or generic-optics with 'productCode' instead." #-}

instance Lude.FromJSON Entitlement where
  parseJSON =
    Lude.withObject
      "Entitlement"
      ( \x ->
          Entitlement'
            Lude.<$> (x Lude..:? "Dimension")
            Lude.<*> (x Lude..:? "Value")
            Lude.<*> (x Lude..:? "ExpirationDate")
            Lude.<*> (x Lude..:? "CustomerIdentifier")
            Lude.<*> (x Lude..:? "ProductCode")
      )
