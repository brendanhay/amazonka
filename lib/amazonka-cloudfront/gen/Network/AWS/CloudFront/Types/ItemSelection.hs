{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ItemSelection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ItemSelection where

import Network.AWS.Prelude

data ItemSelection
  = ISAll
  | ISNone
  | ISWhitelist
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

instance FromText ItemSelection where
  parser =
    takeLowerText >>= \case
      "all" -> pure ISAll
      "none" -> pure ISNone
      "whitelist" -> pure ISWhitelist
      e ->
        fromTextError $
          "Failure parsing ItemSelection from value: '" <> e
            <> "'. Accepted values: all, none, whitelist"

instance ToText ItemSelection where
  toText = \case
    ISAll -> "all"
    ISNone -> "none"
    ISWhitelist -> "whitelist"

instance Hashable ItemSelection

instance NFData ItemSelection

instance ToByteString ItemSelection

instance ToQuery ItemSelection

instance ToHeader ItemSelection

instance FromXML ItemSelection where
  parseXML = parseXMLText "ItemSelection"

instance ToXML ItemSelection where
  toXML = toXMLText
