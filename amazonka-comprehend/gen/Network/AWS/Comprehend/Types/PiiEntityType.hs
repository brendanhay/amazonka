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
-- Module      : Network.AWS.Comprehend.Types.PiiEntityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiEntityType
  ( PiiEntityType
      ( ..,
        PiiEntityType_ADDRESS,
        PiiEntityType_AGE,
        PiiEntityType_ALL,
        PiiEntityType_AWS_ACCESS_KEY,
        PiiEntityType_AWS_SECRET_KEY,
        PiiEntityType_BANK_ACCOUNT_NUMBER,
        PiiEntityType_BANK_ROUTING,
        PiiEntityType_CREDIT_DEBIT_CVV,
        PiiEntityType_CREDIT_DEBIT_EXPIRY,
        PiiEntityType_CREDIT_DEBIT_NUMBER,
        PiiEntityType_DATE_TIME,
        PiiEntityType_DRIVER_ID,
        PiiEntityType_EMAIL,
        PiiEntityType_IP_ADDRESS,
        PiiEntityType_MAC_ADDRESS,
        PiiEntityType_NAME,
        PiiEntityType_PASSPORT_NUMBER,
        PiiEntityType_PASSWORD,
        PiiEntityType_PHONE,
        PiiEntityType_PIN,
        PiiEntityType_SSN,
        PiiEntityType_URL,
        PiiEntityType_USERNAME
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype PiiEntityType = PiiEntityType'
  { fromPiiEntityType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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

pattern PiiEntityType_IP_ADDRESS :: PiiEntityType
pattern PiiEntityType_IP_ADDRESS = PiiEntityType' "IP_ADDRESS"

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

pattern PiiEntityType_URL :: PiiEntityType
pattern PiiEntityType_URL = PiiEntityType' "URL"

pattern PiiEntityType_USERNAME :: PiiEntityType
pattern PiiEntityType_USERNAME = PiiEntityType' "USERNAME"

{-# COMPLETE
  PiiEntityType_ADDRESS,
  PiiEntityType_AGE,
  PiiEntityType_ALL,
  PiiEntityType_AWS_ACCESS_KEY,
  PiiEntityType_AWS_SECRET_KEY,
  PiiEntityType_BANK_ACCOUNT_NUMBER,
  PiiEntityType_BANK_ROUTING,
  PiiEntityType_CREDIT_DEBIT_CVV,
  PiiEntityType_CREDIT_DEBIT_EXPIRY,
  PiiEntityType_CREDIT_DEBIT_NUMBER,
  PiiEntityType_DATE_TIME,
  PiiEntityType_DRIVER_ID,
  PiiEntityType_EMAIL,
  PiiEntityType_IP_ADDRESS,
  PiiEntityType_MAC_ADDRESS,
  PiiEntityType_NAME,
  PiiEntityType_PASSPORT_NUMBER,
  PiiEntityType_PASSWORD,
  PiiEntityType_PHONE,
  PiiEntityType_PIN,
  PiiEntityType_SSN,
  PiiEntityType_URL,
  PiiEntityType_USERNAME,
  PiiEntityType'
  #-}
