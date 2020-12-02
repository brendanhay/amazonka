{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Attribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Attribute where

import Network.AWS.Prelude

data Attribute
  = All
  | Default
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

instance FromText Attribute where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      "default" -> pure Default
      e ->
        fromTextError $
          "Failure parsing Attribute from value: '" <> e
            <> "'. Accepted values: all, default"

instance ToText Attribute where
  toText = \case
    All -> "ALL"
    Default -> "DEFAULT"

instance Hashable Attribute

instance NFData Attribute

instance ToByteString Attribute

instance ToQuery Attribute

instance ToHeader Attribute

instance ToJSON Attribute where
  toJSON = toJSONText
