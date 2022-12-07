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
-- Module      : Amazonka.Transcribe.Types.PiiEntityType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.PiiEntityType
  ( PiiEntityType
      ( ..,
        PiiEntityType_ADDRESS,
        PiiEntityType_ALL,
        PiiEntityType_BANK_ACCOUNT_NUMBER,
        PiiEntityType_BANK_ROUTING,
        PiiEntityType_CREDIT_DEBIT_CVV,
        PiiEntityType_CREDIT_DEBIT_EXPIRY,
        PiiEntityType_CREDIT_DEBIT_NUMBER,
        PiiEntityType_EMAIL,
        PiiEntityType_NAME,
        PiiEntityType_PHONE,
        PiiEntityType_PIN,
        PiiEntityType_SSN
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

pattern PiiEntityType_ALL :: PiiEntityType
pattern PiiEntityType_ALL = PiiEntityType' "ALL"

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

pattern PiiEntityType_EMAIL :: PiiEntityType
pattern PiiEntityType_EMAIL = PiiEntityType' "EMAIL"

pattern PiiEntityType_NAME :: PiiEntityType
pattern PiiEntityType_NAME = PiiEntityType' "NAME"

pattern PiiEntityType_PHONE :: PiiEntityType
pattern PiiEntityType_PHONE = PiiEntityType' "PHONE"

pattern PiiEntityType_PIN :: PiiEntityType
pattern PiiEntityType_PIN = PiiEntityType' "PIN"

pattern PiiEntityType_SSN :: PiiEntityType
pattern PiiEntityType_SSN = PiiEntityType' "SSN"

{-# COMPLETE
  PiiEntityType_ADDRESS,
  PiiEntityType_ALL,
  PiiEntityType_BANK_ACCOUNT_NUMBER,
  PiiEntityType_BANK_ROUTING,
  PiiEntityType_CREDIT_DEBIT_CVV,
  PiiEntityType_CREDIT_DEBIT_EXPIRY,
  PiiEntityType_CREDIT_DEBIT_NUMBER,
  PiiEntityType_EMAIL,
  PiiEntityType_NAME,
  PiiEntityType_PHONE,
  PiiEntityType_PIN,
  PiiEntityType_SSN,
  PiiEntityType'
  #-}
