{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ResourceShareType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ResourceShareType where

import Network.AWS.Prelude

data ResourceShareType
  = All
  | Foreign
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

instance FromText ResourceShareType where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      "foreign" -> pure Foreign
      e ->
        fromTextError $
          "Failure parsing ResourceShareType from value: '" <> e
            <> "'. Accepted values: all, foreign"

instance ToText ResourceShareType where
  toText = \case
    All -> "ALL"
    Foreign -> "FOREIGN"

instance Hashable ResourceShareType

instance NFData ResourceShareType

instance ToByteString ResourceShareType

instance ToQuery ResourceShareType

instance ToHeader ResourceShareType

instance ToJSON ResourceShareType where
  toJSON = toJSONText
