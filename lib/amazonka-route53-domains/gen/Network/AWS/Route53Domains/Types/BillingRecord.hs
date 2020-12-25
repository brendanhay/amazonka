{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.BillingRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.BillingRecord
  ( BillingRecord (..),

    -- * Smart constructor
    mkBillingRecord,

    -- * Lenses
    brBillDate,
    brDomainName,
    brInvoiceId,
    brOperation,
    brPrice,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53Domains.Types.DomainName as Types
import qualified Network.AWS.Route53Domains.Types.InvoiceId as Types
import qualified Network.AWS.Route53Domains.Types.OperationType as Types

-- | Information for one billing record.
--
-- /See:/ 'mkBillingRecord' smart constructor.
data BillingRecord = BillingRecord'
  { -- | The date that the operation was billed, in Unix format.
    billDate :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the domain that the billing record applies to. If the domain name contains characters other than a-z, 0-9, and - (hyphen), such as an internationalized domain name, then this value is in Punycode. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format> in the /Amazon Route 53 Developer Guide/ .
    domainName :: Core.Maybe Types.DomainName,
    -- | The ID of the invoice that is associated with the billing record.
    invoiceId :: Core.Maybe Types.InvoiceId,
    -- | The operation that you were charged for.
    operation :: Core.Maybe Types.OperationType,
    -- | The price that you were charged for the operation, in US dollars.
    --
    -- Example value: 12.0
    price :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BillingRecord' value with any optional fields omitted.
mkBillingRecord ::
  BillingRecord
mkBillingRecord =
  BillingRecord'
    { billDate = Core.Nothing,
      domainName = Core.Nothing,
      invoiceId = Core.Nothing,
      operation = Core.Nothing,
      price = Core.Nothing
    }

-- | The date that the operation was billed, in Unix format.
--
-- /Note:/ Consider using 'billDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brBillDate :: Lens.Lens' BillingRecord (Core.Maybe Core.NominalDiffTime)
brBillDate = Lens.field @"billDate"
{-# DEPRECATED brBillDate "Use generic-lens or generic-optics with 'billDate' instead." #-}

-- | The name of the domain that the billing record applies to. If the domain name contains characters other than a-z, 0-9, and - (hyphen), such as an internationalized domain name, then this value is in Punycode. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format> in the /Amazon Route 53 Developer Guide/ .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brDomainName :: Lens.Lens' BillingRecord (Core.Maybe Types.DomainName)
brDomainName = Lens.field @"domainName"
{-# DEPRECATED brDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The ID of the invoice that is associated with the billing record.
--
-- /Note:/ Consider using 'invoiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brInvoiceId :: Lens.Lens' BillingRecord (Core.Maybe Types.InvoiceId)
brInvoiceId = Lens.field @"invoiceId"
{-# DEPRECATED brInvoiceId "Use generic-lens or generic-optics with 'invoiceId' instead." #-}

-- | The operation that you were charged for.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brOperation :: Lens.Lens' BillingRecord (Core.Maybe Types.OperationType)
brOperation = Lens.field @"operation"
{-# DEPRECATED brOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The price that you were charged for the operation, in US dollars.
--
-- Example value: 12.0
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brPrice :: Lens.Lens' BillingRecord (Core.Maybe Core.Double)
brPrice = Lens.field @"price"
{-# DEPRECATED brPrice "Use generic-lens or generic-optics with 'price' instead." #-}

instance Core.FromJSON BillingRecord where
  parseJSON =
    Core.withObject "BillingRecord" Core.$
      \x ->
        BillingRecord'
          Core.<$> (x Core..:? "BillDate")
          Core.<*> (x Core..:? "DomainName")
          Core.<*> (x Core..:? "InvoiceId")
          Core.<*> (x Core..:? "Operation")
          Core.<*> (x Core..:? "Price")
