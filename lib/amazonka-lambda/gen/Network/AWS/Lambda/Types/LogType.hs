{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LogType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LogType where

import Network.AWS.Prelude

data LogType
  = None
  | Tail
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

instance FromText LogType where
  parser =
    takeLowerText >>= \case
      "none" -> pure None
      "tail" -> pure Tail
      e ->
        fromTextError $
          "Failure parsing LogType from value: '" <> e
            <> "'. Accepted values: none, tail"

instance ToText LogType where
  toText = \case
    None -> "None"
    Tail -> "Tail"

instance Hashable LogType

instance NFData LogType

instance ToByteString LogType

instance ToQuery LogType

instance ToHeader LogType

instance ToJSON LogType where
  toJSON = toJSONText
