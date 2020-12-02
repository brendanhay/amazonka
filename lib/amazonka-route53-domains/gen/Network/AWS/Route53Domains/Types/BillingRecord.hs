{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.BillingRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.BillingRecord where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53Domains.Types.OperationType

-- | Information for one billing record.
--
--
--
-- /See:/ 'billingRecord' smart constructor.
data BillingRecord = BillingRecord'
  { _brOperation ::
      !(Maybe OperationType),
    _brInvoiceId :: !(Maybe Text),
    _brDomainName :: !(Maybe Text),
    _brBillDate :: !(Maybe POSIX),
    _brPrice :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BillingRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brOperation' - The operation that you were charged for.
--
-- * 'brInvoiceId' - The ID of the invoice that is associated with the billing record.
--
-- * 'brDomainName' - The name of the domain that the billing record applies to. If the domain name contains characters other than a-z, 0-9, and - (hyphen), such as an internationalized domain name, then this value is in Punycode. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format> in the /Amazon Route 53 Developer Guide/ .
--
-- * 'brBillDate' - The date that the operation was billed, in Unix format.
--
-- * 'brPrice' - The price that you were charged for the operation, in US dollars. Example value: 12.0
billingRecord ::
  BillingRecord
billingRecord =
  BillingRecord'
    { _brOperation = Nothing,
      _brInvoiceId = Nothing,
      _brDomainName = Nothing,
      _brBillDate = Nothing,
      _brPrice = Nothing
    }

-- | The operation that you were charged for.
brOperation :: Lens' BillingRecord (Maybe OperationType)
brOperation = lens _brOperation (\s a -> s {_brOperation = a})

-- | The ID of the invoice that is associated with the billing record.
brInvoiceId :: Lens' BillingRecord (Maybe Text)
brInvoiceId = lens _brInvoiceId (\s a -> s {_brInvoiceId = a})

-- | The name of the domain that the billing record applies to. If the domain name contains characters other than a-z, 0-9, and - (hyphen), such as an internationalized domain name, then this value is in Punycode. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format> in the /Amazon Route 53 Developer Guide/ .
brDomainName :: Lens' BillingRecord (Maybe Text)
brDomainName = lens _brDomainName (\s a -> s {_brDomainName = a})

-- | The date that the operation was billed, in Unix format.
brBillDate :: Lens' BillingRecord (Maybe UTCTime)
brBillDate = lens _brBillDate (\s a -> s {_brBillDate = a}) . mapping _Time

-- | The price that you were charged for the operation, in US dollars. Example value: 12.0
brPrice :: Lens' BillingRecord (Maybe Double)
brPrice = lens _brPrice (\s a -> s {_brPrice = a})

instance FromJSON BillingRecord where
  parseJSON =
    withObject
      "BillingRecord"
      ( \x ->
          BillingRecord'
            <$> (x .:? "Operation")
            <*> (x .:? "InvoiceId")
            <*> (x .:? "DomainName")
            <*> (x .:? "BillDate")
            <*> (x .:? "Price")
      )

instance Hashable BillingRecord

instance NFData BillingRecord
