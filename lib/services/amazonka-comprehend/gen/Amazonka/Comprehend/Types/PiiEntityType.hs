{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Comprehend.Types.PiiEntityType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.PiiEntityType
  ( PiiEntityType
      ( ..,
        PiiEntityType_ADDRESS,
        PiiEntityType_AGE,
        PiiEntityType_ALL,
        PiiEntityType_AWS_ACCESS_KEY,
        PiiEntityType_AWS_SECRET_KEY,
        PiiEntityType_BANK_ACCOUNT_NUMBER,
        PiiEntityType_BANK_ROUTING,
        PiiEntityType_CA_HEALTH_NUMBER,
        PiiEntityType_CA_SOCIAL_INSURANCE_NUMBER,
        PiiEntityType_CREDIT_DEBIT_CVV,
        PiiEntityType_CREDIT_DEBIT_EXPIRY,
        PiiEntityType_CREDIT_DEBIT_NUMBER,
        PiiEntityType_DATE_TIME,
        PiiEntityType_DRIVER_ID,
        PiiEntityType_EMAIL,
        PiiEntityType_INTERNATIONAL_BANK_ACCOUNT_NUMBER,
        PiiEntityType_IN_AADHAAR,
        PiiEntityType_IN_NREGA,
        PiiEntityType_IN_PERMANENT_ACCOUNT_NUMBER,
        PiiEntityType_IN_VOTER_NUMBER,
        PiiEntityType_IP_ADDRESS,
        PiiEntityType_LICENSE_PLATE,
        PiiEntityType_MAC_ADDRESS,
        PiiEntityType_NAME,
        PiiEntityType_PASSPORT_NUMBER,
        PiiEntityType_PASSWORD,
        PiiEntityType_PHONE,
        PiiEntityType_PIN,
        PiiEntityType_SSN,
        PiiEntityType_SWIFT_CODE,
        PiiEntityType_UK_NATIONAL_HEALTH_SERVICE_NUMBER,
        PiiEntityType_UK_NATIONAL_INSURANCE_NUMBER,
        PiiEntityType_UK_UNIQUE_TAXPAYER_REFERENCE_NUMBER,
        PiiEntityType_URL,
        PiiEntityType_USERNAME,
        PiiEntityType_US_INDIVIDUAL_TAX_IDENTIFICATION_NUMBER,
        PiiEntityType_VEHICLE_IDENTIFICATION_NUMBER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PiiEntityType = PiiEntityType'
  { fromPiiEntityType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern PiiEntityType_ADDRESS :: PiiEntityType
pattern PiiEntityType_ADDRESS = PiiEntityType' "ADDRESS"

pattern PiiEntityType_AGE :: PiiEntityType
pattern PiiEntityType_AGE = PiiEntityType' "AGE"

pattern PiiEntityType_ALL :: PiiEntityType
pattern PiiEntityType_ALL = PiiEntityType' "ALL"

pattern PiiEntityType_AWS_ACCESS_KEY :: PiiEntityType
pattern PiiEntityType_AWS_ACCESS_KEY = PiiEntityType' "AWS_ACCESS_KEY"

pattern PiiEntityType_AWS_SECRET_KEY :: PiiEntityType
pattern PiiEntityType_AWS_SECRET_KEY = PiiEntityType' "AWS_SECRET_KEY"

pattern PiiEntityType_BANK_ACCOUNT_NUMBER :: PiiEntityType
pattern PiiEntityType_BANK_ACCOUNT_NUMBER = PiiEntityType' "BANK_ACCOUNT_NUMBER"

pattern PiiEntityType_BANK_ROUTING :: PiiEntityType
pattern PiiEntityType_BANK_ROUTING = PiiEntityType' "BANK_ROUTING"

pattern PiiEntityType_CA_HEALTH_NUMBER :: PiiEntityType
pattern PiiEntityType_CA_HEALTH_NUMBER = PiiEntityType' "CA_HEALTH_NUMBER"

pattern PiiEntityType_CA_SOCIAL_INSURANCE_NUMBER :: PiiEntityType
pattern PiiEntityType_CA_SOCIAL_INSURANCE_NUMBER = PiiEntityType' "CA_SOCIAL_INSURANCE_NUMBER"

pattern PiiEntityType_CREDIT_DEBIT_CVV :: PiiEntityType
pattern PiiEntityType_CREDIT_DEBIT_CVV = PiiEntityType' "CREDIT_DEBIT_CVV"

pattern PiiEntityType_CREDIT_DEBIT_EXPIRY :: PiiEntityType
pattern PiiEntityType_CREDIT_DEBIT_EXPIRY = PiiEntityType' "CREDIT_DEBIT_EXPIRY"

pattern PiiEntityType_CREDIT_DEBIT_NUMBER :: PiiEntityType
pattern PiiEntityType_CREDIT_DEBIT_NUMBER = PiiEntityType' "CREDIT_DEBIT_NUMBER"

pattern PiiEntityType_DATE_TIME :: PiiEntityType
pattern PiiEntityType_DATE_TIME = PiiEntityType' "DATE_TIME"

pattern PiiEntityType_DRIVER_ID :: PiiEntityType
pattern PiiEntityType_DRIVER_ID = PiiEntityType' "DRIVER_ID"

pattern PiiEntityType_EMAIL :: PiiEntityType
pattern PiiEntityType_EMAIL = PiiEntityType' "EMAIL"

pattern PiiEntityType_INTERNATIONAL_BANK_ACCOUNT_NUMBER :: PiiEntityType
pattern PiiEntityType_INTERNATIONAL_BANK_ACCOUNT_NUMBER = PiiEntityType' "INTERNATIONAL_BANK_ACCOUNT_NUMBER"

pattern PiiEntityType_IN_AADHAAR :: PiiEntityType
pattern PiiEntityType_IN_AADHAAR = PiiEntityType' "IN_AADHAAR"

pattern PiiEntityType_IN_NREGA :: PiiEntityType
pattern PiiEntityType_IN_NREGA = PiiEntityType' "IN_NREGA"

pattern PiiEntityType_IN_PERMANENT_ACCOUNT_NUMBER :: PiiEntityType
pattern PiiEntityType_IN_PERMANENT_ACCOUNT_NUMBER = PiiEntityType' "IN_PERMANENT_ACCOUNT_NUMBER"

pattern PiiEntityType_IN_VOTER_NUMBER :: PiiEntityType
pattern PiiEntityType_IN_VOTER_NUMBER = PiiEntityType' "IN_VOTER_NUMBER"

pattern PiiEntityType_IP_ADDRESS :: PiiEntityType
pattern PiiEntityType_IP_ADDRESS = PiiEntityType' "IP_ADDRESS"

pattern PiiEntityType_LICENSE_PLATE :: PiiEntityType
pattern PiiEntityType_LICENSE_PLATE = PiiEntityType' "LICENSE_PLATE"

pattern PiiEntityType_MAC_ADDRESS :: PiiEntityType
pattern PiiEntityType_MAC_ADDRESS = PiiEntityType' "MAC_ADDRESS"

pattern PiiEntityType_NAME :: PiiEntityType
pattern PiiEntityType_NAME = PiiEntityType' "NAME"

pattern PiiEntityType_PASSPORT_NUMBER :: PiiEntityType
pattern PiiEntityType_PASSPORT_NUMBER = PiiEntityType' "PASSPORT_NUMBER"

pattern PiiEntityType_PASSWORD :: PiiEntityType
pattern PiiEntityType_PASSWORD = PiiEntityType' "PASSWORD"

pattern PiiEntityType_PHONE :: PiiEntityType
pattern PiiEntityType_PHONE = PiiEntityType' "PHONE"

pattern PiiEntityType_PIN :: PiiEntityType
pattern PiiEntityType_PIN = PiiEntityType' "PIN"

pattern PiiEntityType_SSN :: PiiEntityType
pattern PiiEntityType_SSN = PiiEntityType' "SSN"

pattern PiiEntityType_SWIFT_CODE :: PiiEntityType
pattern PiiEntityType_SWIFT_CODE = PiiEntityType' "SWIFT_CODE"

pattern PiiEntityType_UK_NATIONAL_HEALTH_SERVICE_NUMBER :: PiiEntityType
pattern PiiEntityType_UK_NATIONAL_HEALTH_SERVICE_NUMBER = PiiEntityType' "UK_NATIONAL_HEALTH_SERVICE_NUMBER"

pattern PiiEntityType_UK_NATIONAL_INSURANCE_NUMBER :: PiiEntityType
pattern PiiEntityType_UK_NATIONAL_INSURANCE_NUMBER = PiiEntityType' "UK_NATIONAL_INSURANCE_NUMBER"

pattern PiiEntityType_UK_UNIQUE_TAXPAYER_REFERENCE_NUMBER :: PiiEntityType
pattern PiiEntityType_UK_UNIQUE_TAXPAYER_REFERENCE_NUMBER = PiiEntityType' "UK_UNIQUE_TAXPAYER_REFERENCE_NUMBER"

pattern PiiEntityType_URL :: PiiEntityType
pattern PiiEntityType_URL = PiiEntityType' "URL"

pattern PiiEntityType_USERNAME :: PiiEntityType
pattern PiiEntityType_USERNAME = PiiEntityType' "USERNAME"

pattern PiiEntityType_US_INDIVIDUAL_TAX_IDENTIFICATION_NUMBER :: PiiEntityType
pattern PiiEntityType_US_INDIVIDUAL_TAX_IDENTIFICATION_NUMBER = PiiEntityType' "US_INDIVIDUAL_TAX_IDENTIFICATION_NUMBER"

pattern PiiEntityType_VEHICLE_IDENTIFICATION_NUMBER :: PiiEntityType
pattern PiiEntityType_VEHICLE_IDENTIFICATION_NUMBER = PiiEntityType' "VEHICLE_IDENTIFICATION_NUMBER"

{-# COMPLETE
  PiiEntityType_ADDRESS,
  PiiEntityType_AGE,
  PiiEntityType_ALL,
  PiiEntityType_AWS_ACCESS_KEY,
  PiiEntityType_AWS_SECRET_KEY,
  PiiEntityType_BANK_ACCOUNT_NUMBER,
  PiiEntityType_BANK_ROUTING,
  PiiEntityType_CA_HEALTH_NUMBER,
  PiiEntityType_CA_SOCIAL_INSURANCE_NUMBER,
  PiiEntityType_CREDIT_DEBIT_CVV,
  PiiEntityType_CREDIT_DEBIT_EXPIRY,
  PiiEntityType_CREDIT_DEBIT_NUMBER,
  PiiEntityType_DATE_TIME,
  PiiEntityType_DRIVER_ID,
  PiiEntityType_EMAIL,
  PiiEntityType_INTERNATIONAL_BANK_ACCOUNT_NUMBER,
  PiiEntityType_IN_AADHAAR,
  PiiEntityType_IN_NREGA,
  PiiEntityType_IN_PERMANENT_ACCOUNT_NUMBER,
  PiiEntityType_IN_VOTER_NUMBER,
  PiiEntityType_IP_ADDRESS,
  PiiEntityType_LICENSE_PLATE,
  PiiEntityType_MAC_ADDRESS,
  PiiEntityType_NAME,
  PiiEntityType_PASSPORT_NUMBER,
  PiiEntityType_PASSWORD,
  PiiEntityType_PHONE,
  PiiEntityType_PIN,
  PiiEntityType_SSN,
  PiiEntityType_SWIFT_CODE,
  PiiEntityType_UK_NATIONAL_HEALTH_SERVICE_NUMBER,
  PiiEntityType_UK_NATIONAL_INSURANCE_NUMBER,
  PiiEntityType_UK_UNIQUE_TAXPAYER_REFERENCE_NUMBER,
  PiiEntityType_URL,
  PiiEntityType_USERNAME,
  PiiEntityType_US_INDIVIDUAL_TAX_IDENTIFICATION_NUMBER,
  PiiEntityType_VEHICLE_IDENTIFICATION_NUMBER,
  PiiEntityType'
  #-}
