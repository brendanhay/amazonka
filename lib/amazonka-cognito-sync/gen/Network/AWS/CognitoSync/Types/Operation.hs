{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.Operation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.Operation where

import Network.AWS.Prelude

data Operation
  = Remove
  | Replace
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

instance FromText Operation where
  parser =
    takeLowerText >>= \case
      "remove" -> pure Remove
      "replace" -> pure Replace
      e ->
        fromTextError $
          "Failure parsing Operation from value: '" <> e
            <> "'. Accepted values: remove, replace"

instance ToText Operation where
  toText = \case
    Remove -> "remove"
    Replace -> "replace"

instance Hashable Operation

instance NFData Operation

instance ToByteString Operation

instance ToQuery Operation

instance ToHeader Operation

instance ToJSON Operation where
  toJSON = toJSONText
