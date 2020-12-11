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
        Archived,
        Compromised,
        Confirmed,
        ForceChangePassword,
        ResetRequired,
        Unconfirmed,
        Unknown
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype UserStatusType = UserStatusType' Lude.Text
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

pattern Archived :: UserStatusType
pattern Archived = UserStatusType' "ARCHIVED"

pattern Compromised :: UserStatusType
pattern Compromised = UserStatusType' "COMPROMISED"

pattern Confirmed :: UserStatusType
pattern Confirmed = UserStatusType' "CONFIRMED"

pattern ForceChangePassword :: UserStatusType
pattern ForceChangePassword = UserStatusType' "FORCE_CHANGE_PASSWORD"

pattern ResetRequired :: UserStatusType
pattern ResetRequired = UserStatusType' "RESET_REQUIRED"

pattern Unconfirmed :: UserStatusType
pattern Unconfirmed = UserStatusType' "UNCONFIRMED"

pattern Unknown :: UserStatusType
pattern Unknown = UserStatusType' "UNKNOWN"

{-# COMPLETE
  Archived,
  Compromised,
  Confirmed,
  ForceChangePassword,
  ResetRequired,
  Unconfirmed,
  Unknown,
  UserStatusType'
  #-}
