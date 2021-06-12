{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.BillingRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.BillingRecord where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Route53Domains.Types.OperationType

-- | Information for one billing record.
--
-- /See:/ 'newBillingRecord' smart constructor.
data BillingRecord = BillingRecord'
  { -- | The ID of the invoice that is associated with the billing record.
    invoiceId :: Core.Maybe Core.Text,
    -- | The operation that you were charged for.
    operation :: Core.Maybe OperationType,
    -- | The name of the domain that the billing record applies to. If the domain
    -- name contains characters other than a-z, 0-9, and - (hyphen), such as an
    -- internationalized domain name, then this value is in Punycode. For more
    -- information, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format>
    -- in the /Amazon Route 53 Developer Guide/.
    domainName :: Core.Maybe Core.Text,
    -- | The date that the operation was billed, in Unix format.
    billDate :: Core.Maybe Core.POSIX,
    -- | The price that you were charged for the operation, in US dollars.
    --
    -- Example value: 12.0
    price :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BillingRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invoiceId', 'billingRecord_invoiceId' - The ID of the invoice that is associated with the billing record.
--
-- 'operation', 'billingRecord_operation' - The operation that you were charged for.
--
-- 'domainName', 'billingRecord_domainName' - The name of the domain that the billing record applies to. If the domain
-- name contains characters other than a-z, 0-9, and - (hyphen), such as an
-- internationalized domain name, then this value is in Punycode. For more
-- information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format>
-- in the /Amazon Route 53 Developer Guide/.
--
-- 'billDate', 'billingRecord_billDate' - The date that the operation was billed, in Unix format.
--
-- 'price', 'billingRecord_price' - The price that you were charged for the operation, in US dollars.
--
-- Example value: 12.0
newBillingRecord ::
  BillingRecord
newBillingRecord =
  BillingRecord'
    { invoiceId = Core.Nothing,
      operation = Core.Nothing,
      domainName = Core.Nothing,
      billDate = Core.Nothing,
      price = Core.Nothing
    }

-- | The ID of the invoice that is associated with the billing record.
billingRecord_invoiceId :: Lens.Lens' BillingRecord (Core.Maybe Core.Text)
billingRecord_invoiceId = Lens.lens (\BillingRecord' {invoiceId} -> invoiceId) (\s@BillingRecord' {} a -> s {invoiceId = a} :: BillingRecord)

-- | The operation that you were charged for.
billingRecord_operation :: Lens.Lens' BillingRecord (Core.Maybe OperationType)
billingRecord_operation = Lens.lens (\BillingRecord' {operation} -> operation) (\s@BillingRecord' {} a -> s {operation = a} :: BillingRecord)

-- | The name of the domain that the billing record applies to. If the domain
-- name contains characters other than a-z, 0-9, and - (hyphen), such as an
-- internationalized domain name, then this value is in Punycode. For more
-- information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format>
-- in the /Amazon Route 53 Developer Guide/.
billingRecord_domainName :: Lens.Lens' BillingRecord (Core.Maybe Core.Text)
billingRecord_domainName = Lens.lens (\BillingRecord' {domainName} -> domainName) (\s@BillingRecord' {} a -> s {domainName = a} :: BillingRecord)

-- | The date that the operation was billed, in Unix format.
billingRecord_billDate :: Lens.Lens' BillingRecord (Core.Maybe Core.UTCTime)
billingRecord_billDate = Lens.lens (\BillingRecord' {billDate} -> billDate) (\s@BillingRecord' {} a -> s {billDate = a} :: BillingRecord) Core.. Lens.mapping Core._Time

-- | The price that you were charged for the operation, in US dollars.
--
-- Example value: 12.0
billingRecord_price :: Lens.Lens' BillingRecord (Core.Maybe Core.Double)
billingRecord_price = Lens.lens (\BillingRecord' {price} -> price) (\s@BillingRecord' {} a -> s {price = a} :: BillingRecord)

instance Core.FromJSON BillingRecord where
  parseJSON =
    Core.withObject
      "BillingRecord"
      ( \x ->
          BillingRecord'
            Core.<$> (x Core..:? "InvoiceId")
            Core.<*> (x Core..:? "Operation")
            Core.<*> (x Core..:? "DomainName")
            Core.<*> (x Core..:? "BillDate")
            Core.<*> (x Core..:? "Price")
      )

instance Core.Hashable BillingRecord

instance Core.NFData BillingRecord
