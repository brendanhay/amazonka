-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PiiEntityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiEntityType
  ( PiiEntityType
      ( PiiEntityType',
        AWSAccessKey,
        AWSSecretKey,
        Address,
        Age,
        All,
        BankAccountNumber,
        BankRouting,
        CreditDebitCvv,
        CreditDebitExpiry,
        CreditDebitNumber,
        DateTime,
        DriverId,
        Email,
        IPAddress,
        MACAddress,
        Name,
        PassportNumber,
        Password,
        Phone,
        Pin,
        Ssn,
        URL,
        Username
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PiiEntityType = PiiEntityType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AWSAccessKey :: PiiEntityType
pattern AWSAccessKey = PiiEntityType' "AWS_ACCESS_KEY"

pattern AWSSecretKey :: PiiEntityType
pattern AWSSecretKey = PiiEntityType' "AWS_SECRET_KEY"

pattern Address :: PiiEntityType
pattern Address = PiiEntityType' "ADDRESS"

pattern Age :: PiiEntityType
pattern Age = PiiEntityType' "AGE"

pattern All :: PiiEntityType
pattern All = PiiEntityType' "ALL"

pattern BankAccountNumber :: PiiEntityType
pattern BankAccountNumber = PiiEntityType' "BANK_ACCOUNT_NUMBER"

pattern BankRouting :: PiiEntityType
pattern BankRouting = PiiEntityType' "BANK_ROUTING"

pattern CreditDebitCvv :: PiiEntityType
pattern CreditDebitCvv = PiiEntityType' "CREDIT_DEBIT_CVV"

pattern CreditDebitExpiry :: PiiEntityType
pattern CreditDebitExpiry = PiiEntityType' "CREDIT_DEBIT_EXPIRY"

pattern CreditDebitNumber :: PiiEntityType
pattern CreditDebitNumber = PiiEntityType' "CREDIT_DEBIT_NUMBER"

pattern DateTime :: PiiEntityType
pattern DateTime = PiiEntityType' "DATE_TIME"

pattern DriverId :: PiiEntityType
pattern DriverId = PiiEntityType' "DRIVER_ID"

pattern Email :: PiiEntityType
pattern Email = PiiEntityType' "EMAIL"

pattern IPAddress :: PiiEntityType
pattern IPAddress = PiiEntityType' "IP_ADDRESS"

pattern MACAddress :: PiiEntityType
pattern MACAddress = PiiEntityType' "MAC_ADDRESS"

pattern Name :: PiiEntityType
pattern Name = PiiEntityType' "NAME"

pattern PassportNumber :: PiiEntityType
pattern PassportNumber = PiiEntityType' "PASSPORT_NUMBER"

pattern Password :: PiiEntityType
pattern Password = PiiEntityType' "PASSWORD"

pattern Phone :: PiiEntityType
pattern Phone = PiiEntityType' "PHONE"

pattern Pin :: PiiEntityType
pattern Pin = PiiEntityType' "PIN"

pattern Ssn :: PiiEntityType
pattern Ssn = PiiEntityType' "SSN"

pattern URL :: PiiEntityType
pattern URL = PiiEntityType' "URL"

pattern Username :: PiiEntityType
pattern Username = PiiEntityType' "USERNAME"

{-# COMPLETE
  AWSAccessKey,
  AWSSecretKey,
  Address,
  Age,
  All,
  BankAccountNumber,
  BankRouting,
  CreditDebitCvv,
  CreditDebitExpiry,
  CreditDebitNumber,
  DateTime,
  DriverId,
  Email,
  IPAddress,
  MACAddress,
  Name,
  PassportNumber,
  Password,
  Phone,
  Pin,
  Ssn,
  URL,
  Username,
  PiiEntityType'
  #-}
