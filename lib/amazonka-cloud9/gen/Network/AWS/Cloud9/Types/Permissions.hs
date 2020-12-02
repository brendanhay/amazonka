{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.Permissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.Permissions where

import Network.AWS.Prelude

data Permissions
  = Owner
  | ReadOnly
  | ReadWrite
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

instance FromText Permissions where
  parser =
    takeLowerText >>= \case
      "owner" -> pure Owner
      "read-only" -> pure ReadOnly
      "read-write" -> pure ReadWrite
      e ->
        fromTextError $
          "Failure parsing Permissions from value: '" <> e
            <> "'. Accepted values: owner, read-only, read-write"

instance ToText Permissions where
  toText = \case
    Owner -> "owner"
    ReadOnly -> "read-only"
    ReadWrite -> "read-write"

instance Hashable Permissions

instance NFData Permissions

instance ToByteString Permissions

instance ToQuery Permissions

instance ToHeader Permissions

instance ToJSON Permissions where
  toJSON = toJSONText

instance FromJSON Permissions where
  parseJSON = parseJSONText "Permissions"
