{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserStatusType
  ( UserStatusType
      ( UserStatusType',
        UserStatusTypeUnconfirmed,
        UserStatusTypeConfirmed,
        UserStatusTypeArchived,
        UserStatusTypeCompromised,
        UserStatusTypeUnknown,
        UserStatusTypeResetRequired,
        UserStatusTypeForceChangePassword,
        fromUserStatusType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype UserStatusType = UserStatusType'
  { fromUserStatusType ::
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

pattern UserStatusTypeUnconfirmed :: UserStatusType
pattern UserStatusTypeUnconfirmed = UserStatusType' "UNCONFIRMED"

pattern UserStatusTypeConfirmed :: UserStatusType
pattern UserStatusTypeConfirmed = UserStatusType' "CONFIRMED"

pattern UserStatusTypeArchived :: UserStatusType
pattern UserStatusTypeArchived = UserStatusType' "ARCHIVED"

pattern UserStatusTypeCompromised :: UserStatusType
pattern UserStatusTypeCompromised = UserStatusType' "COMPROMISED"

pattern UserStatusTypeUnknown :: UserStatusType
pattern UserStatusTypeUnknown = UserStatusType' "UNKNOWN"

pattern UserStatusTypeResetRequired :: UserStatusType
pattern UserStatusTypeResetRequired = UserStatusType' "RESET_REQUIRED"

pattern UserStatusTypeForceChangePassword :: UserStatusType
pattern UserStatusTypeForceChangePassword = UserStatusType' "FORCE_CHANGE_PASSWORD"

{-# COMPLETE
  UserStatusTypeUnconfirmed,
  UserStatusTypeConfirmed,
  UserStatusTypeArchived,
  UserStatusTypeCompromised,
  UserStatusTypeUnknown,
  UserStatusTypeResetRequired,
  UserStatusTypeForceChangePassword,
  UserStatusType'
  #-}
