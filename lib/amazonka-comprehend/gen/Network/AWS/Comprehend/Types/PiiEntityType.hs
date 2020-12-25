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
        PiiEntityTypeBankAccountNumber,
        PiiEntityTypeBankRouting,
        PiiEntityTypeCreditDebitNumber,
        PiiEntityTypeCreditDebitCvv,
        PiiEntityTypeCreditDebitExpiry,
        PiiEntityTypePin,
        PiiEntityTypeEmail,
        PiiEntityTypeAddress,
        PiiEntityTypeName,
        PiiEntityTypePhone,
        PiiEntityTypeSsn,
        PiiEntityTypeDateTime,
        PiiEntityTypePassportNumber,
        PiiEntityTypeDriverId,
        PiiEntityTypeUrl,
        PiiEntityTypeAge,
        PiiEntityTypeUsername,
        PiiEntityTypePassword,
        PiiEntityTypeAwsAccessKey,
        PiiEntityTypeAwsSecretKey,
        PiiEntityTypeIpAddress,
        PiiEntityTypeMacAddress,
        PiiEntityTypeAll,
        fromPiiEntityType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype PiiEntityType = PiiEntityType'
  { fromPiiEntityType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern PiiEntityTypeBankAccountNumber :: PiiEntityType
pattern PiiEntityTypeBankAccountNumber = PiiEntityType' "BANK_ACCOUNT_NUMBER"

pattern PiiEntityTypeBankRouting :: PiiEntityType
pattern PiiEntityTypeBankRouting = PiiEntityType' "BANK_ROUTING"

pattern PiiEntityTypeCreditDebitNumber :: PiiEntityType
pattern PiiEntityTypeCreditDebitNumber = PiiEntityType' "CREDIT_DEBIT_NUMBER"

pattern PiiEntityTypeCreditDebitCvv :: PiiEntityType
pattern PiiEntityTypeCreditDebitCvv = PiiEntityType' "CREDIT_DEBIT_CVV"

pattern PiiEntityTypeCreditDebitExpiry :: PiiEntityType
pattern PiiEntityTypeCreditDebitExpiry = PiiEntityType' "CREDIT_DEBIT_EXPIRY"

pattern PiiEntityTypePin :: PiiEntityType
pattern PiiEntityTypePin = PiiEntityType' "PIN"

pattern PiiEntityTypeEmail :: PiiEntityType
pattern PiiEntityTypeEmail = PiiEntityType' "EMAIL"

pattern PiiEntityTypeAddress :: PiiEntityType
pattern PiiEntityTypeAddress = PiiEntityType' "ADDRESS"

pattern PiiEntityTypeName :: PiiEntityType
pattern PiiEntityTypeName = PiiEntityType' "NAME"

pattern PiiEntityTypePhone :: PiiEntityType
pattern PiiEntityTypePhone = PiiEntityType' "PHONE"

pattern PiiEntityTypeSsn :: PiiEntityType
pattern PiiEntityTypeSsn = PiiEntityType' "SSN"

pattern PiiEntityTypeDateTime :: PiiEntityType
pattern PiiEntityTypeDateTime = PiiEntityType' "DATE_TIME"

pattern PiiEntityTypePassportNumber :: PiiEntityType
pattern PiiEntityTypePassportNumber = PiiEntityType' "PASSPORT_NUMBER"

pattern PiiEntityTypeDriverId :: PiiEntityType
pattern PiiEntityTypeDriverId = PiiEntityType' "DRIVER_ID"

pattern PiiEntityTypeUrl :: PiiEntityType
pattern PiiEntityTypeUrl = PiiEntityType' "URL"

pattern PiiEntityTypeAge :: PiiEntityType
pattern PiiEntityTypeAge = PiiEntityType' "AGE"

pattern PiiEntityTypeUsername :: PiiEntityType
pattern PiiEntityTypeUsername = PiiEntityType' "USERNAME"

pattern PiiEntityTypePassword :: PiiEntityType
pattern PiiEntityTypePassword = PiiEntityType' "PASSWORD"

pattern PiiEntityTypeAwsAccessKey :: PiiEntityType
pattern PiiEntityTypeAwsAccessKey = PiiEntityType' "AWS_ACCESS_KEY"

pattern PiiEntityTypeAwsSecretKey :: PiiEntityType
pattern PiiEntityTypeAwsSecretKey = PiiEntityType' "AWS_SECRET_KEY"

pattern PiiEntityTypeIpAddress :: PiiEntityType
pattern PiiEntityTypeIpAddress = PiiEntityType' "IP_ADDRESS"

pattern PiiEntityTypeMacAddress :: PiiEntityType
pattern PiiEntityTypeMacAddress = PiiEntityType' "MAC_ADDRESS"

pattern PiiEntityTypeAll :: PiiEntityType
pattern PiiEntityTypeAll = PiiEntityType' "ALL"

{-# COMPLETE
  PiiEntityTypeBankAccountNumber,
  PiiEntityTypeBankRouting,
  PiiEntityTypeCreditDebitNumber,
  PiiEntityTypeCreditDebitCvv,
  PiiEntityTypeCreditDebitExpiry,
  PiiEntityTypePin,
  PiiEntityTypeEmail,
  PiiEntityTypeAddress,
  PiiEntityTypeName,
  PiiEntityTypePhone,
  PiiEntityTypeSsn,
  PiiEntityTypeDateTime,
  PiiEntityTypePassportNumber,
  PiiEntityTypeDriverId,
  PiiEntityTypeUrl,
  PiiEntityTypeAge,
  PiiEntityTypeUsername,
  PiiEntityTypePassword,
  PiiEntityTypeAwsAccessKey,
  PiiEntityTypeAwsSecretKey,
  PiiEntityTypeIpAddress,
  PiiEntityTypeMacAddress,
  PiiEntityTypeAll,
  PiiEntityType'
  #-}
