{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PiiEntityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiEntityType where

import Network.AWS.Prelude

data PiiEntityType
  = AWSAccessKey
  | AWSSecretKey
  | Address
  | Age
  | All
  | BankAccountNumber
  | BankRouting
  | CreditDebitCvv
  | CreditDebitExpiry
  | CreditDebitNumber
  | DateTime
  | DriverId
  | Email
  | IPAddress
  | MACAddress
  | Name
  | PassportNumber
  | Password
  | Phone
  | Pin
  | Ssn
  | URL
  | Username
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText PiiEntityType where
  parser =
    takeLowerText >>= \case
      "aws_access_key" -> pure AWSAccessKey
      "aws_secret_key" -> pure AWSSecretKey
      "address" -> pure Address
      "age" -> pure Age
      "all" -> pure All
      "bank_account_number" -> pure BankAccountNumber
      "bank_routing" -> pure BankRouting
      "credit_debit_cvv" -> pure CreditDebitCvv
      "credit_debit_expiry" -> pure CreditDebitExpiry
      "credit_debit_number" -> pure CreditDebitNumber
      "date_time" -> pure DateTime
      "driver_id" -> pure DriverId
      "email" -> pure Email
      "ip_address" -> pure IPAddress
      "mac_address" -> pure MACAddress
      "name" -> pure Name
      "passport_number" -> pure PassportNumber
      "password" -> pure Password
      "phone" -> pure Phone
      "pin" -> pure Pin
      "ssn" -> pure Ssn
      "url" -> pure URL
      "username" -> pure Username
      e ->
        fromTextError $
          "Failure parsing PiiEntityType from value: '" <> e
            <> "'. Accepted values: aws_access_key, aws_secret_key, address, age, all, bank_account_number, bank_routing, credit_debit_cvv, credit_debit_expiry, credit_debit_number, date_time, driver_id, email, ip_address, mac_address, name, passport_number, password, phone, pin, ssn, url, username"

instance ToText PiiEntityType where
  toText = \case
    AWSAccessKey -> "AWS_ACCESS_KEY"
    AWSSecretKey -> "AWS_SECRET_KEY"
    Address -> "ADDRESS"
    Age -> "AGE"
    All -> "ALL"
    BankAccountNumber -> "BANK_ACCOUNT_NUMBER"
    BankRouting -> "BANK_ROUTING"
    CreditDebitCvv -> "CREDIT_DEBIT_CVV"
    CreditDebitExpiry -> "CREDIT_DEBIT_EXPIRY"
    CreditDebitNumber -> "CREDIT_DEBIT_NUMBER"
    DateTime -> "DATE_TIME"
    DriverId -> "DRIVER_ID"
    Email -> "EMAIL"
    IPAddress -> "IP_ADDRESS"
    MACAddress -> "MAC_ADDRESS"
    Name -> "NAME"
    PassportNumber -> "PASSPORT_NUMBER"
    Password -> "PASSWORD"
    Phone -> "PHONE"
    Pin -> "PIN"
    Ssn -> "SSN"
    URL -> "URL"
    Username -> "USERNAME"

instance Hashable PiiEntityType

instance NFData PiiEntityType

instance ToByteString PiiEntityType

instance ToQuery PiiEntityType

instance ToHeader PiiEntityType

instance ToJSON PiiEntityType where
  toJSON = toJSONText

instance FromJSON PiiEntityType where
  parseJSON = parseJSONText "PiiEntityType"
