{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EntityStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EntityStatusCode where

import Network.AWS.Prelude

data EntityStatusCode
  = Impaired
  | Unimpaired
  | Unknown
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

instance FromText EntityStatusCode where
  parser =
    takeLowerText >>= \case
      "impaired" -> pure Impaired
      "unimpaired" -> pure Unimpaired
      "unknown" -> pure Unknown
      e ->
        fromTextError $
          "Failure parsing EntityStatusCode from value: '" <> e
            <> "'. Accepted values: impaired, unimpaired, unknown"

instance ToText EntityStatusCode where
  toText = \case
    Impaired -> "IMPAIRED"
    Unimpaired -> "UNIMPAIRED"
    Unknown -> "UNKNOWN"

instance Hashable EntityStatusCode

instance NFData EntityStatusCode

instance ToByteString EntityStatusCode

instance ToQuery EntityStatusCode

instance ToHeader EntityStatusCode

instance ToJSON EntityStatusCode where
  toJSON = toJSONText

instance FromJSON EntityStatusCode where
  parseJSON = parseJSONText "EntityStatusCode"
