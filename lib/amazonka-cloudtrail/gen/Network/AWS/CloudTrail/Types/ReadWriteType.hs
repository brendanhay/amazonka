{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.ReadWriteType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.ReadWriteType where

import Network.AWS.Prelude

data ReadWriteType
  = RWTAll
  | RWTReadOnly
  | RWTWriteOnly
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

instance FromText ReadWriteType where
  parser =
    takeLowerText >>= \case
      "all" -> pure RWTAll
      "readonly" -> pure RWTReadOnly
      "writeonly" -> pure RWTWriteOnly
      e ->
        fromTextError $
          "Failure parsing ReadWriteType from value: '" <> e
            <> "'. Accepted values: all, readonly, writeonly"

instance ToText ReadWriteType where
  toText = \case
    RWTAll -> "All"
    RWTReadOnly -> "ReadOnly"
    RWTWriteOnly -> "WriteOnly"

instance Hashable ReadWriteType

instance NFData ReadWriteType

instance ToByteString ReadWriteType

instance ToQuery ReadWriteType

instance ToHeader ReadWriteType

instance ToJSON ReadWriteType where
  toJSON = toJSONText

instance FromJSON ReadWriteType where
  parseJSON = parseJSONText "ReadWriteType"
