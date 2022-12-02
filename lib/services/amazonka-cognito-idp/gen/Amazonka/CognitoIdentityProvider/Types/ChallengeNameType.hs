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
-- Module      : Amazonka.CognitoIdentityProvider.Types.ChallengeNameType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.ChallengeNameType
  ( ChallengeNameType
      ( ..,
        ChallengeNameType_ADMIN_NO_SRP_AUTH,
        ChallengeNameType_CUSTOM_CHALLENGE,
        ChallengeNameType_DEVICE_PASSWORD_VERIFIER,
        ChallengeNameType_DEVICE_SRP_AUTH,
        ChallengeNameType_MFA_SETUP,
        ChallengeNameType_NEW_PASSWORD_REQUIRED,
        ChallengeNameType_PASSWORD_VERIFIER,
        ChallengeNameType_SELECT_MFA_TYPE,
        ChallengeNameType_SMS_MFA,
        ChallengeNameType_SOFTWARE_TOKEN_MFA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChallengeNameType = ChallengeNameType'
  { fromChallengeNameType ::
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

pattern ChallengeNameType_ADMIN_NO_SRP_AUTH :: ChallengeNameType
pattern ChallengeNameType_ADMIN_NO_SRP_AUTH = ChallengeNameType' "ADMIN_NO_SRP_AUTH"

pattern ChallengeNameType_CUSTOM_CHALLENGE :: ChallengeNameType
pattern ChallengeNameType_CUSTOM_CHALLENGE = ChallengeNameType' "CUSTOM_CHALLENGE"

pattern ChallengeNameType_DEVICE_PASSWORD_VERIFIER :: ChallengeNameType
pattern ChallengeNameType_DEVICE_PASSWORD_VERIFIER = ChallengeNameType' "DEVICE_PASSWORD_VERIFIER"

pattern ChallengeNameType_DEVICE_SRP_AUTH :: ChallengeNameType
pattern ChallengeNameType_DEVICE_SRP_AUTH = ChallengeNameType' "DEVICE_SRP_AUTH"

pattern ChallengeNameType_MFA_SETUP :: ChallengeNameType
pattern ChallengeNameType_MFA_SETUP = ChallengeNameType' "MFA_SETUP"

pattern ChallengeNameType_NEW_PASSWORD_REQUIRED :: ChallengeNameType
pattern ChallengeNameType_NEW_PASSWORD_REQUIRED = ChallengeNameType' "NEW_PASSWORD_REQUIRED"

pattern ChallengeNameType_PASSWORD_VERIFIER :: ChallengeNameType
pattern ChallengeNameType_PASSWORD_VERIFIER = ChallengeNameType' "PASSWORD_VERIFIER"

pattern ChallengeNameType_SELECT_MFA_TYPE :: ChallengeNameType
pattern ChallengeNameType_SELECT_MFA_TYPE = ChallengeNameType' "SELECT_MFA_TYPE"

pattern ChallengeNameType_SMS_MFA :: ChallengeNameType
pattern ChallengeNameType_SMS_MFA = ChallengeNameType' "SMS_MFA"

pattern ChallengeNameType_SOFTWARE_TOKEN_MFA :: ChallengeNameType
pattern ChallengeNameType_SOFTWARE_TOKEN_MFA = ChallengeNameType' "SOFTWARE_TOKEN_MFA"

{-# COMPLETE
  ChallengeNameType_ADMIN_NO_SRP_AUTH,
  ChallengeNameType_CUSTOM_CHALLENGE,
  ChallengeNameType_DEVICE_PASSWORD_VERIFIER,
  ChallengeNameType_DEVICE_SRP_AUTH,
  ChallengeNameType_MFA_SETUP,
  ChallengeNameType_NEW_PASSWORD_REQUIRED,
  ChallengeNameType_PASSWORD_VERIFIER,
  ChallengeNameType_SELECT_MFA_TYPE,
  ChallengeNameType_SMS_MFA,
  ChallengeNameType_SOFTWARE_TOKEN_MFA,
  ChallengeNameType'
  #-}
