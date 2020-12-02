{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Operator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Operator where

import Network.AWS.Prelude

data Operator
  = OAll
  | OAny
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

instance FromText Operator where
  parser =
    takeLowerText >>= \case
      "all" -> pure OAll
      "any" -> pure OAny
      e ->
        fromTextError $
          "Failure parsing Operator from value: '" <> e
            <> "'. Accepted values: all, any"

instance ToText Operator where
  toText = \case
    OAll -> "ALL"
    OAny -> "ANY"

instance Hashable Operator

instance NFData Operator

instance ToByteString Operator

instance ToQuery Operator

instance ToHeader Operator

instance ToJSON Operator where
  toJSON = toJSONText

instance FromJSON Operator where
  parseJSON = parseJSONText "Operator"
