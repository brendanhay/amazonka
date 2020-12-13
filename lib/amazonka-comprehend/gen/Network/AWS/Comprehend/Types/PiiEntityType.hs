{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        BankAccountNumber,
        BankRouting,
        CreditDebitNumber,
        CreditDebitCvv,
        CreditDebitExpiry,
        Pin,
        Email,
        Address,
        Name,
        Phone,
        Ssn,
        DateTime,
        PassportNumber,
        DriverId,
        URL,
        Age,
        Username,
        Password,
        AWSAccessKey,
        AWSSecretKey,
        IPAddress,
        MACAddress,
        All
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

pattern BankAccountNumber :: PiiEntityType
pattern BankAccountNumber = PiiEntityType' "BANK_ACCOUNT_NUMBER"

pattern BankRouting :: PiiEntityType
pattern BankRouting = PiiEntityType' "BANK_ROUTING"

pattern CreditDebitNumber :: PiiEntityType
pattern CreditDebitNumber = PiiEntityType' "CREDIT_DEBIT_NUMBER"

pattern CreditDebitCvv :: PiiEntityType
pattern CreditDebitCvv = PiiEntityType' "CREDIT_DEBIT_CVV"

pattern CreditDebitExpiry :: PiiEntityType
pattern CreditDebitExpiry = PiiEntityType' "CREDIT_DEBIT_EXPIRY"

pattern Pin :: PiiEntityType
pattern Pin = PiiEntityType' "PIN"

pattern Email :: PiiEntityType
pattern Email = PiiEntityType' "EMAIL"

pattern Address :: PiiEntityType
pattern Address = PiiEntityType' "ADDRESS"

pattern Name :: PiiEntityType
pattern Name = PiiEntityType' "NAME"

pattern Phone :: PiiEntityType
pattern Phone = PiiEntityType' "PHONE"

pattern Ssn :: PiiEntityType
pattern Ssn = PiiEntityType' "SSN"

pattern DateTime :: PiiEntityType
pattern DateTime = PiiEntityType' "DATE_TIME"

pattern PassportNumber :: PiiEntityType
pattern PassportNumber = PiiEntityType' "PASSPORT_NUMBER"

pattern DriverId :: PiiEntityType
pattern DriverId = PiiEntityType' "DRIVER_ID"

pattern URL :: PiiEntityType
pattern URL = PiiEntityType' "URL"

pattern Age :: PiiEntityType
pattern Age = PiiEntityType' "AGE"

pattern Username :: PiiEntityType
pattern Username = PiiEntityType' "USERNAME"

pattern Password :: PiiEntityType
pattern Password = PiiEntityType' "PASSWORD"

pattern AWSAccessKey :: PiiEntityType
pattern AWSAccessKey = PiiEntityType' "AWS_ACCESS_KEY"

pattern AWSSecretKey :: PiiEntityType
pattern AWSSecretKey = PiiEntityType' "AWS_SECRET_KEY"

pattern IPAddress :: PiiEntityType
pattern IPAddress = PiiEntityType' "IP_ADDRESS"

pattern MACAddress :: PiiEntityType
pattern MACAddress = PiiEntityType' "MAC_ADDRESS"

pattern All :: PiiEntityType
pattern All = PiiEntityType' "ALL"

{-# COMPLETE
  BankAccountNumber,
  BankRouting,
  CreditDebitNumber,
  CreditDebitCvv,
  CreditDebitExpiry,
  Pin,
  Email,
  Address,
  Name,
  Phone,
  Ssn,
  DateTime,
  PassportNumber,
  DriverId,
  URL,
  Age,
  Username,
  Password,
  AWSAccessKey,
  AWSSecretKey,
  IPAddress,
  MACAddress,
  All,
  PiiEntityType'
  #-}
