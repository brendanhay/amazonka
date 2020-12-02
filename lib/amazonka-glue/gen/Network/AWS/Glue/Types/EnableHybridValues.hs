{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.EnableHybridValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.EnableHybridValues where

import Network.AWS.Prelude

data EnableHybridValues
  = False'
  | True'
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

instance FromText EnableHybridValues where
  parser =
    takeLowerText >>= \case
      "false" -> pure False'
      "true" -> pure True'
      e ->
        fromTextError $
          "Failure parsing EnableHybridValues from value: '" <> e
            <> "'. Accepted values: false, true"

instance ToText EnableHybridValues where
  toText = \case
    False' -> "FALSE"
    True' -> "TRUE"

instance Hashable EnableHybridValues

instance NFData EnableHybridValues

instance ToByteString EnableHybridValues

instance ToQuery EnableHybridValues

instance ToHeader EnableHybridValues

instance ToJSON EnableHybridValues where
  toJSON = toJSONText
