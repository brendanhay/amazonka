{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.PackageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.PackageType where

import Network.AWS.Prelude

data PackageType = TxtDictionary
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

instance FromText PackageType where
  parser =
    takeLowerText >>= \case
      "txt-dictionary" -> pure TxtDictionary
      e ->
        fromTextError $
          "Failure parsing PackageType from value: '" <> e
            <> "'. Accepted values: txt-dictionary"

instance ToText PackageType where
  toText = \case
    TxtDictionary -> "TXT-DICTIONARY"

instance Hashable PackageType

instance NFData PackageType

instance ToByteString PackageType

instance ToQuery PackageType

instance ToHeader PackageType

instance ToJSON PackageType where
  toJSON = toJSONText

instance FromJSON PackageType where
  parseJSON = parseJSONText "PackageType"
