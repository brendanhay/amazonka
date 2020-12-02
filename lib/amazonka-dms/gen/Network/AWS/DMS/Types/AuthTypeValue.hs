{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.AuthTypeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.AuthTypeValue where

import Network.AWS.Prelude

data AuthTypeValue
  = NO
  | Password
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

instance FromText AuthTypeValue where
  parser =
    takeLowerText >>= \case
      "no" -> pure NO
      "password" -> pure Password
      e ->
        fromTextError $
          "Failure parsing AuthTypeValue from value: '" <> e
            <> "'. Accepted values: no, password"

instance ToText AuthTypeValue where
  toText = \case
    NO -> "no"
    Password -> "password"

instance Hashable AuthTypeValue

instance NFData AuthTypeValue

instance ToByteString AuthTypeValue

instance ToQuery AuthTypeValue

instance ToHeader AuthTypeValue

instance ToJSON AuthTypeValue where
  toJSON = toJSONText

instance FromJSON AuthTypeValue where
  parseJSON = parseJSONText "AuthTypeValue"
