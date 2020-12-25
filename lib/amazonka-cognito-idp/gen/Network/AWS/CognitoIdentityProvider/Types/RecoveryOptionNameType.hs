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
        RecoveryOptionNameTypeVerifiedEmail,
        RecoveryOptionNameTypeVerifiedPhoneNumber,
        RecoveryOptionNameTypeAdminOnly,
        fromRecoveryOptionNameType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype RecoveryOptionNameType = RecoveryOptionNameType'
  { fromRecoveryOptionNameType ::
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

pattern RecoveryOptionNameTypeVerifiedEmail :: RecoveryOptionNameType
pattern RecoveryOptionNameTypeVerifiedEmail = RecoveryOptionNameType' "verified_email"

pattern RecoveryOptionNameTypeVerifiedPhoneNumber :: RecoveryOptionNameType
pattern RecoveryOptionNameTypeVerifiedPhoneNumber = RecoveryOptionNameType' "verified_phone_number"

pattern RecoveryOptionNameTypeAdminOnly :: RecoveryOptionNameType
pattern RecoveryOptionNameTypeAdminOnly = RecoveryOptionNameType' "admin_only"

{-# COMPLETE
  RecoveryOptionNameTypeVerifiedEmail,
  RecoveryOptionNameTypeVerifiedPhoneNumber,
  RecoveryOptionNameTypeAdminOnly,
  RecoveryOptionNameType'
  #-}
