{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Type
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Type where

import Network.AWS.Prelude

data Type
  = TAll
  | TAny
  | TNone
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

instance FromText Type where
  parser =
    takeLowerText >>= \case
      "all" -> pure TAll
      "any" -> pure TAny
      "none" -> pure TNone
      e ->
        fromTextError $
          "Failure parsing Type from value: '" <> e
            <> "'. Accepted values: all, any, none"

instance ToText Type where
  toText = \case
    TAll -> "ALL"
    TAny -> "ANY"
    TNone -> "NONE"

instance Hashable Type

instance NFData Type

instance ToByteString Type

instance ToQuery Type

instance ToHeader Type

instance ToJSON Type where
  toJSON = toJSONText

instance FromJSON Type where
  parseJSON = parseJSONText "Type"
