{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.TrustType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.TrustType where

import Network.AWS.Prelude

data TrustType
  = External
  | Forest
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

instance FromText TrustType where
  parser =
    takeLowerText >>= \case
      "external" -> pure External
      "forest" -> pure Forest
      e ->
        fromTextError $
          "Failure parsing TrustType from value: '" <> e
            <> "'. Accepted values: external, forest"

instance ToText TrustType where
  toText = \case
    External -> "External"
    Forest -> "Forest"

instance Hashable TrustType

instance NFData TrustType

instance ToByteString TrustType

instance ToQuery TrustType

instance ToHeader TrustType

instance ToJSON TrustType where
  toJSON = toJSONText

instance FromJSON TrustType where
  parseJSON = parseJSONText "TrustType"
