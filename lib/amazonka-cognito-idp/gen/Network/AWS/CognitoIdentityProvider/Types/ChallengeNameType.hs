{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ChallengeNameType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ChallengeNameType where

import Network.AWS.Prelude

data ChallengeNameType
  = CNTAdminNoSrpAuth
  | CNTCustomChallenge
  | CNTDevicePasswordVerifier
  | CNTDeviceSrpAuth
  | CNTMFASetup
  | CNTNewPasswordRequired
  | CNTPasswordVerifier
  | CNTSelectMFAType
  | CNTSmsMFA
  | CNTSoftwareTokenMFA
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

instance FromText ChallengeNameType where
  parser =
    takeLowerText >>= \case
      "admin_no_srp_auth" -> pure CNTAdminNoSrpAuth
      "custom_challenge" -> pure CNTCustomChallenge
      "device_password_verifier" -> pure CNTDevicePasswordVerifier
      "device_srp_auth" -> pure CNTDeviceSrpAuth
      "mfa_setup" -> pure CNTMFASetup
      "new_password_required" -> pure CNTNewPasswordRequired
      "password_verifier" -> pure CNTPasswordVerifier
      "select_mfa_type" -> pure CNTSelectMFAType
      "sms_mfa" -> pure CNTSmsMFA
      "software_token_mfa" -> pure CNTSoftwareTokenMFA
      e ->
        fromTextError $
          "Failure parsing ChallengeNameType from value: '" <> e
            <> "'. Accepted values: admin_no_srp_auth, custom_challenge, device_password_verifier, device_srp_auth, mfa_setup, new_password_required, password_verifier, select_mfa_type, sms_mfa, software_token_mfa"

instance ToText ChallengeNameType where
  toText = \case
    CNTAdminNoSrpAuth -> "ADMIN_NO_SRP_AUTH"
    CNTCustomChallenge -> "CUSTOM_CHALLENGE"
    CNTDevicePasswordVerifier -> "DEVICE_PASSWORD_VERIFIER"
    CNTDeviceSrpAuth -> "DEVICE_SRP_AUTH"
    CNTMFASetup -> "MFA_SETUP"
    CNTNewPasswordRequired -> "NEW_PASSWORD_REQUIRED"
    CNTPasswordVerifier -> "PASSWORD_VERIFIER"
    CNTSelectMFAType -> "SELECT_MFA_TYPE"
    CNTSmsMFA -> "SMS_MFA"
    CNTSoftwareTokenMFA -> "SOFTWARE_TOKEN_MFA"

instance Hashable ChallengeNameType

instance NFData ChallengeNameType

instance ToByteString ChallengeNameType

instance ToQuery ChallengeNameType

instance ToHeader ChallengeNameType

instance ToJSON ChallengeNameType where
  toJSON = toJSONText

instance FromJSON ChallengeNameType where
  parseJSON = parseJSONText "ChallengeNameType"
