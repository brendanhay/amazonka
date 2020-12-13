{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ChallengeNameType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ChallengeNameType
  ( ChallengeNameType
      ( ChallengeNameType',
        SmsMFA,
        SoftwareTokenMFA,
        SelectMFAType,
        MFASetup,
        PasswordVerifier,
        CustomChallenge,
        DeviceSrpAuth,
        DevicePasswordVerifier,
        AdminNoSrpAuth,
        NewPasswordRequired
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ChallengeNameType = ChallengeNameType' Lude.Text
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

pattern SmsMFA :: ChallengeNameType
pattern SmsMFA = ChallengeNameType' "SMS_MFA"

pattern SoftwareTokenMFA :: ChallengeNameType
pattern SoftwareTokenMFA = ChallengeNameType' "SOFTWARE_TOKEN_MFA"

pattern SelectMFAType :: ChallengeNameType
pattern SelectMFAType = ChallengeNameType' "SELECT_MFA_TYPE"

pattern MFASetup :: ChallengeNameType
pattern MFASetup = ChallengeNameType' "MFA_SETUP"

pattern PasswordVerifier :: ChallengeNameType
pattern PasswordVerifier = ChallengeNameType' "PASSWORD_VERIFIER"

pattern CustomChallenge :: ChallengeNameType
pattern CustomChallenge = ChallengeNameType' "CUSTOM_CHALLENGE"

pattern DeviceSrpAuth :: ChallengeNameType
pattern DeviceSrpAuth = ChallengeNameType' "DEVICE_SRP_AUTH"

pattern DevicePasswordVerifier :: ChallengeNameType
pattern DevicePasswordVerifier = ChallengeNameType' "DEVICE_PASSWORD_VERIFIER"

pattern AdminNoSrpAuth :: ChallengeNameType
pattern AdminNoSrpAuth = ChallengeNameType' "ADMIN_NO_SRP_AUTH"

pattern NewPasswordRequired :: ChallengeNameType
pattern NewPasswordRequired = ChallengeNameType' "NEW_PASSWORD_REQUIRED"

{-# COMPLETE
  SmsMFA,
  SoftwareTokenMFA,
  SelectMFAType,
  MFASetup,
  PasswordVerifier,
  CustomChallenge,
  DeviceSrpAuth,
  DevicePasswordVerifier,
  AdminNoSrpAuth,
  NewPasswordRequired,
  ChallengeNameType'
  #-}
