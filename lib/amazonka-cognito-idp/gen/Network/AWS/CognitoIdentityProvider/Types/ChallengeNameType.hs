{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ChallengeNameType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.ChallengeNameType
  ( ChallengeNameType
    ( ChallengeNameType'
    , ChallengeNameTypeSmsMfa
    , ChallengeNameTypeSoftwareTokenMfa
    , ChallengeNameTypeSelectMfaType
    , ChallengeNameTypeMfaSetup
    , ChallengeNameTypePasswordVerifier
    , ChallengeNameTypeCustomChallenge
    , ChallengeNameTypeDeviceSrpAuth
    , ChallengeNameTypeDevicePasswordVerifier
    , ChallengeNameTypeAdminNoSrpAuth
    , ChallengeNameTypeNewPasswordRequired
    , fromChallengeNameType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ChallengeNameType = ChallengeNameType'{fromChallengeNameType
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern ChallengeNameTypeSmsMfa :: ChallengeNameType
pattern ChallengeNameTypeSmsMfa = ChallengeNameType' "SMS_MFA"

pattern ChallengeNameTypeSoftwareTokenMfa :: ChallengeNameType
pattern ChallengeNameTypeSoftwareTokenMfa = ChallengeNameType' "SOFTWARE_TOKEN_MFA"

pattern ChallengeNameTypeSelectMfaType :: ChallengeNameType
pattern ChallengeNameTypeSelectMfaType = ChallengeNameType' "SELECT_MFA_TYPE"

pattern ChallengeNameTypeMfaSetup :: ChallengeNameType
pattern ChallengeNameTypeMfaSetup = ChallengeNameType' "MFA_SETUP"

pattern ChallengeNameTypePasswordVerifier :: ChallengeNameType
pattern ChallengeNameTypePasswordVerifier = ChallengeNameType' "PASSWORD_VERIFIER"

pattern ChallengeNameTypeCustomChallenge :: ChallengeNameType
pattern ChallengeNameTypeCustomChallenge = ChallengeNameType' "CUSTOM_CHALLENGE"

pattern ChallengeNameTypeDeviceSrpAuth :: ChallengeNameType
pattern ChallengeNameTypeDeviceSrpAuth = ChallengeNameType' "DEVICE_SRP_AUTH"

pattern ChallengeNameTypeDevicePasswordVerifier :: ChallengeNameType
pattern ChallengeNameTypeDevicePasswordVerifier = ChallengeNameType' "DEVICE_PASSWORD_VERIFIER"

pattern ChallengeNameTypeAdminNoSrpAuth :: ChallengeNameType
pattern ChallengeNameTypeAdminNoSrpAuth = ChallengeNameType' "ADMIN_NO_SRP_AUTH"

pattern ChallengeNameTypeNewPasswordRequired :: ChallengeNameType
pattern ChallengeNameTypeNewPasswordRequired = ChallengeNameType' "NEW_PASSWORD_REQUIRED"

{-# COMPLETE 
  ChallengeNameTypeSmsMfa,

  ChallengeNameTypeSoftwareTokenMfa,

  ChallengeNameTypeSelectMfaType,

  ChallengeNameTypeMfaSetup,

  ChallengeNameTypePasswordVerifier,

  ChallengeNameTypeCustomChallenge,

  ChallengeNameTypeDeviceSrpAuth,

  ChallengeNameTypeDevicePasswordVerifier,

  ChallengeNameTypeAdminNoSrpAuth,

  ChallengeNameTypeNewPasswordRequired,
  ChallengeNameType'
  #-}
