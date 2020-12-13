{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionNameType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionNameType
  ( RecoveryOptionNameType
      ( RecoveryOptionNameType',
        VerifiedEmail,
        VerifiedPhoneNumber,
        AdminOnly
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RecoveryOptionNameType = RecoveryOptionNameType' Lude.Text
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

pattern VerifiedEmail :: RecoveryOptionNameType
pattern VerifiedEmail = RecoveryOptionNameType' "verified_email"

pattern VerifiedPhoneNumber :: RecoveryOptionNameType
pattern VerifiedPhoneNumber = RecoveryOptionNameType' "verified_phone_number"

pattern AdminOnly :: RecoveryOptionNameType
pattern AdminOnly = RecoveryOptionNameType' "admin_only"

{-# COMPLETE
  VerifiedEmail,
  VerifiedPhoneNumber,
  AdminOnly,
  RecoveryOptionNameType'
  #-}
