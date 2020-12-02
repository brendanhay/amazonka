{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.MemberType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.MemberType where

import Network.AWS.Prelude

data MemberType
  = Group
  | User
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

instance FromText MemberType where
  parser =
    takeLowerText >>= \case
      "group" -> pure Group
      "user" -> pure User
      e ->
        fromTextError $
          "Failure parsing MemberType from value: '" <> e
            <> "'. Accepted values: group, user"

instance ToText MemberType where
  toText = \case
    Group -> "GROUP"
    User -> "USER"

instance Hashable MemberType

instance NFData MemberType

instance ToByteString MemberType

instance ToQuery MemberType

instance ToHeader MemberType

instance FromJSON MemberType where
  parseJSON = parseJSONText "MemberType"
