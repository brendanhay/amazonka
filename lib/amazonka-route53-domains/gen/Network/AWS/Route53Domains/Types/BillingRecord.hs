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
    brOperation,
    brInvoiceId,
    brDomainName,
    brBillDate,
    brPrice,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53Domains.Types.OperationType

-- | Information for one billing record.
--
-- /See:/ 'mkBillingRecord' smart constructor.
data BillingRecord = BillingRecord'
  { operation ::
      Lude.Maybe OperationType,
    invoiceId :: Lude.Maybe Lude.Text,
    domainName :: Lude.Maybe Lude.Text,
    billDate :: Lude.Maybe Lude.Timestamp,
    price :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BillingRecord' with the minimum fields required to make a request.
--
-- * 'billDate' - The date that the operation was billed, in Unix format.
-- * 'domainName' - The name of the domain that the billing record applies to. If the domain name contains characters other than a-z, 0-9, and - (hyphen), such as an internationalized domain name, then this value is in Punycode. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format> in the /Amazon Route 53 Developer Guide/ .
-- * 'invoiceId' - The ID of the invoice that is associated with the billing record.
-- * 'operation' - The operation that you were charged for.
-- * 'price' - The price that you were charged for the operation, in US dollars.
--
-- Example value: 12.0
mkBillingRecord ::
  BillingRecord
mkBillingRecord =
  BillingRecord'
    { operation = Lude.Nothing,
      invoiceId = Lude.Nothing,
      domainName = Lude.Nothing,
      billDate = Lude.Nothing,
      price = Lude.Nothing
    }

-- | The operation that you were charged for.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brOperation :: Lens.Lens' BillingRecord (Lude.Maybe OperationType)
brOperation = Lens.lens (operation :: BillingRecord -> Lude.Maybe OperationType) (\s a -> s {operation = a} :: BillingRecord)
{-# DEPRECATED brOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The ID of the invoice that is associated with the billing record.
--
-- /Note:/ Consider using 'invoiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brInvoiceId :: Lens.Lens' BillingRecord (Lude.Maybe Lude.Text)
brInvoiceId = Lens.lens (invoiceId :: BillingRecord -> Lude.Maybe Lude.Text) (\s a -> s {invoiceId = a} :: BillingRecord)
{-# DEPRECATED brInvoiceId "Use generic-lens or generic-optics with 'invoiceId' instead." #-}

-- | The name of the domain that the billing record applies to. If the domain name contains characters other than a-z, 0-9, and - (hyphen), such as an internationalized domain name, then this value is in Punycode. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format> in the /Amazon Route 53 Developer Guide/ .
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brDomainName :: Lens.Lens' BillingRecord (Lude.Maybe Lude.Text)
brDomainName = Lens.lens (domainName :: BillingRecord -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: BillingRecord)
{-# DEPRECATED brDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The date that the operation was billed, in Unix format.
--
-- /Note:/ Consider using 'billDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brBillDate :: Lens.Lens' BillingRecord (Lude.Maybe Lude.Timestamp)
brBillDate = Lens.lens (billDate :: BillingRecord -> Lude.Maybe Lude.Timestamp) (\s a -> s {billDate = a} :: BillingRecord)
{-# DEPRECATED brBillDate "Use generic-lens or generic-optics with 'billDate' instead." #-}

-- | The price that you were charged for the operation, in US dollars.
--
-- Example value: 12.0
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brPrice :: Lens.Lens' BillingRecord (Lude.Maybe Lude.Double)
brPrice = Lens.lens (price :: BillingRecord -> Lude.Maybe Lude.Double) (\s a -> s {price = a} :: BillingRecord)
{-# DEPRECATED brPrice "Use generic-lens or generic-optics with 'price' instead." #-}

instance Lude.FromJSON BillingRecord where
  parseJSON =
    Lude.withObject
      "BillingRecord"
      ( \x ->
          BillingRecord'
            Lude.<$> (x Lude..:? "Operation")
            Lude.<*> (x Lude..:? "InvoiceId")
            Lude.<*> (x Lude..:? "DomainName")
            Lude.<*> (x Lude..:? "BillDate")
            Lude.<*> (x Lude..:? "Price")
      )
