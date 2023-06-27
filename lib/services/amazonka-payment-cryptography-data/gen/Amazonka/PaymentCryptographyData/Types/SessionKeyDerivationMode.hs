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
-- Module      : Amazonka.PaymentCryptographyData.Types.SessionKeyDerivationMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.SessionKeyDerivationMode
  ( SessionKeyDerivationMode
      ( ..,
        SessionKeyDerivationMode_AMEX,
        SessionKeyDerivationMode_EMV2000,
        SessionKeyDerivationMode_EMV_COMMON_SESSION_KEY,
        SessionKeyDerivationMode_MASTERCARD_SESSION_KEY,
        SessionKeyDerivationMode_VISA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SessionKeyDerivationMode = SessionKeyDerivationMode'
  { fromSessionKeyDerivationMode ::
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

pattern SessionKeyDerivationMode_AMEX :: SessionKeyDerivationMode
pattern SessionKeyDerivationMode_AMEX = SessionKeyDerivationMode' "AMEX"

pattern SessionKeyDerivationMode_EMV2000 :: SessionKeyDerivationMode
pattern SessionKeyDerivationMode_EMV2000 = SessionKeyDerivationMode' "EMV2000"

pattern SessionKeyDerivationMode_EMV_COMMON_SESSION_KEY :: SessionKeyDerivationMode
pattern SessionKeyDerivationMode_EMV_COMMON_SESSION_KEY = SessionKeyDerivationMode' "EMV_COMMON_SESSION_KEY"

pattern SessionKeyDerivationMode_MASTERCARD_SESSION_KEY :: SessionKeyDerivationMode
pattern SessionKeyDerivationMode_MASTERCARD_SESSION_KEY = SessionKeyDerivationMode' "MASTERCARD_SESSION_KEY"

pattern SessionKeyDerivationMode_VISA :: SessionKeyDerivationMode
pattern SessionKeyDerivationMode_VISA = SessionKeyDerivationMode' "VISA"

{-# COMPLETE
  SessionKeyDerivationMode_AMEX,
  SessionKeyDerivationMode_EMV2000,
  SessionKeyDerivationMode_EMV_COMMON_SESSION_KEY,
  SessionKeyDerivationMode_MASTERCARD_SESSION_KEY,
  SessionKeyDerivationMode_VISA,
  SessionKeyDerivationMode'
  #-}
