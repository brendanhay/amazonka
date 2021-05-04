{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53Domains.Types.OperationType

-- | Information for one billing record.
--
-- /See:/ 'newBillingRecord' smart constructor.
data BillingRecord = BillingRecord'
  { -- | The ID of the invoice that is associated with the billing record.
    invoiceId :: Prelude.Maybe Prelude.Text,
    -- | The operation that you were charged for.
    operation :: Prelude.Maybe OperationType,
    -- | The name of the domain that the billing record applies to. If the domain
    -- name contains characters other than a-z, 0-9, and - (hyphen), such as an
    -- internationalized domain name, then this value is in Punycode. For more
    -- information, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format>
    -- in the /Amazon Route 53 Developer Guide/.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The date that the operation was billed, in Unix format.
    billDate :: Prelude.Maybe Prelude.POSIX,
    -- | The price that you were charged for the operation, in US dollars.
    --
    -- Example value: 12.0
    price :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { invoiceId = Prelude.Nothing,
      operation = Prelude.Nothing,
      domainName = Prelude.Nothing,
      billDate = Prelude.Nothing,
      price = Prelude.Nothing
    }

-- | The ID of the invoice that is associated with the billing record.
billingRecord_invoiceId :: Lens.Lens' BillingRecord (Prelude.Maybe Prelude.Text)
billingRecord_invoiceId = Lens.lens (\BillingRecord' {invoiceId} -> invoiceId) (\s@BillingRecord' {} a -> s {invoiceId = a} :: BillingRecord)

-- | The operation that you were charged for.
billingRecord_operation :: Lens.Lens' BillingRecord (Prelude.Maybe OperationType)
billingRecord_operation = Lens.lens (\BillingRecord' {operation} -> operation) (\s@BillingRecord' {} a -> s {operation = a} :: BillingRecord)

-- | The name of the domain that the billing record applies to. If the domain
-- name contains characters other than a-z, 0-9, and - (hyphen), such as an
-- internationalized domain name, then this value is in Punycode. For more
-- information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format>
-- in the /Amazon Route 53 Developer Guide/.
billingRecord_domainName :: Lens.Lens' BillingRecord (Prelude.Maybe Prelude.Text)
billingRecord_domainName = Lens.lens (\BillingRecord' {domainName} -> domainName) (\s@BillingRecord' {} a -> s {domainName = a} :: BillingRecord)

-- | The date that the operation was billed, in Unix format.
billingRecord_billDate :: Lens.Lens' BillingRecord (Prelude.Maybe Prelude.UTCTime)
billingRecord_billDate = Lens.lens (\BillingRecord' {billDate} -> billDate) (\s@BillingRecord' {} a -> s {billDate = a} :: BillingRecord) Prelude.. Lens.mapping Prelude._Time

-- | The price that you were charged for the operation, in US dollars.
--
-- Example value: 12.0
billingRecord_price :: Lens.Lens' BillingRecord (Prelude.Maybe Prelude.Double)
billingRecord_price = Lens.lens (\BillingRecord' {price} -> price) (\s@BillingRecord' {} a -> s {price = a} :: BillingRecord)

instance Prelude.FromJSON BillingRecord where
  parseJSON =
    Prelude.withObject
      "BillingRecord"
      ( \x ->
          BillingRecord'
            Prelude.<$> (x Prelude..:? "InvoiceId")
            Prelude.<*> (x Prelude..:? "Operation")
            Prelude.<*> (x Prelude..:? "DomainName")
            Prelude.<*> (x Prelude..:? "BillDate")
            Prelude.<*> (x Prelude..:? "Price")
      )

instance Prelude.Hashable BillingRecord

instance Prelude.NFData BillingRecord
