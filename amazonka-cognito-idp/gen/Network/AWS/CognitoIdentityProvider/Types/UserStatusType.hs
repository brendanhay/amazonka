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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserStatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserStatusType
  ( UserStatusType
      ( ..,
        UserStatusType_ARCHIVED,
        UserStatusType_COMPROMISED,
        UserStatusType_CONFIRMED,
        UserStatusType_FORCE_CHANGE_PASSWORD,
        UserStatusType_RESET_REQUIRED,
        UserStatusType_UNCONFIRMED,
        UserStatusType_UNKNOWN
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype UserStatusType = UserStatusType'
  { fromUserStatusType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern UserStatusType_ARCHIVED :: UserStatusType
pattern UserStatusType_ARCHIVED = UserStatusType' "ARCHIVED"

pattern UserStatusType_COMPROMISED :: UserStatusType
pattern UserStatusType_COMPROMISED = UserStatusType' "COMPROMISED"

pattern UserStatusType_CONFIRMED :: UserStatusType
pattern UserStatusType_CONFIRMED = UserStatusType' "CONFIRMED"

pattern UserStatusType_FORCE_CHANGE_PASSWORD :: UserStatusType
pattern UserStatusType_FORCE_CHANGE_PASSWORD = UserStatusType' "FORCE_CHANGE_PASSWORD"

pattern UserStatusType_RESET_REQUIRED :: UserStatusType
pattern UserStatusType_RESET_REQUIRED = UserStatusType' "RESET_REQUIRED"

pattern UserStatusType_UNCONFIRMED :: UserStatusType
pattern UserStatusType_UNCONFIRMED = UserStatusType' "UNCONFIRMED"

pattern UserStatusType_UNKNOWN :: UserStatusType
pattern UserStatusType_UNKNOWN = UserStatusType' "UNKNOWN"

{-# COMPLETE
  UserStatusType_ARCHIVED,
  UserStatusType_COMPROMISED,
  UserStatusType_CONFIRMED,
  UserStatusType_FORCE_CHANGE_PASSWORD,
  UserStatusType_RESET_REQUIRED,
  UserStatusType_UNCONFIRMED,
  UserStatusType_UNKNOWN,
  UserStatusType'
  #-}
