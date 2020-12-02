{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.IdentityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.IdentityType where

import Network.AWS.Prelude

data IdentityType
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

instance FromText IdentityType where
  parser =
    takeLowerText >>= \case
      "group" -> pure Group
      "user" -> pure User
      e ->
        fromTextError $
          "Failure parsing IdentityType from value: '" <> e
            <> "'. Accepted values: group, user"

instance ToText IdentityType where
  toText = \case
    Group -> "GROUP"
    User -> "USER"

instance Hashable IdentityType

instance NFData IdentityType

instance ToByteString IdentityType

instance ToQuery IdentityType

instance ToHeader IdentityType

instance ToJSON IdentityType where
  toJSON = toJSONText

instance FromJSON IdentityType where
  parseJSON = parseJSONText "IdentityType"
