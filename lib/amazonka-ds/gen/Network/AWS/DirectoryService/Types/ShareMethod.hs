{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.ShareMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.ShareMethod where

import Network.AWS.Prelude

data ShareMethod
  = Handshake
  | Organizations
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

instance FromText ShareMethod where
  parser =
    takeLowerText >>= \case
      "handshake" -> pure Handshake
      "organizations" -> pure Organizations
      e ->
        fromTextError $
          "Failure parsing ShareMethod from value: '" <> e
            <> "'. Accepted values: handshake, organizations"

instance ToText ShareMethod where
  toText = \case
    Handshake -> "HANDSHAKE"
    Organizations -> "ORGANIZATIONS"

instance Hashable ShareMethod

instance NFData ShareMethod

instance ToByteString ShareMethod

instance ToQuery ShareMethod

instance ToHeader ShareMethod

instance ToJSON ShareMethod where
  toJSON = toJSONText

instance FromJSON ShareMethod where
  parseJSON = parseJSONText "ShareMethod"
