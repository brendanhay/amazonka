{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.RegionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.RegionType where

import Network.AWS.Prelude

data RegionType
  = Additional
  | Primary
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

instance FromText RegionType where
  parser =
    takeLowerText >>= \case
      "additional" -> pure Additional
      "primary" -> pure Primary
      e ->
        fromTextError $
          "Failure parsing RegionType from value: '" <> e
            <> "'. Accepted values: additional, primary"

instance ToText RegionType where
  toText = \case
    Additional -> "Additional"
    Primary -> "Primary"

instance Hashable RegionType

instance NFData RegionType

instance ToByteString RegionType

instance ToQuery RegionType

instance ToHeader RegionType

instance FromJSON RegionType where
  parseJSON = parseJSONText "RegionType"
