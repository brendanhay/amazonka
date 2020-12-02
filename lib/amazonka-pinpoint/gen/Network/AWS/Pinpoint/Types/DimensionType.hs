{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.DimensionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.DimensionType where

import Network.AWS.Prelude

data DimensionType
  = DTExclusive
  | DTInclusive
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

instance FromText DimensionType where
  parser =
    takeLowerText >>= \case
      "exclusive" -> pure DTExclusive
      "inclusive" -> pure DTInclusive
      e ->
        fromTextError $
          "Failure parsing DimensionType from value: '" <> e
            <> "'. Accepted values: exclusive, inclusive"

instance ToText DimensionType where
  toText = \case
    DTExclusive -> "EXCLUSIVE"
    DTInclusive -> "INCLUSIVE"

instance Hashable DimensionType

instance NFData DimensionType

instance ToByteString DimensionType

instance ToQuery DimensionType

instance ToHeader DimensionType

instance ToJSON DimensionType where
  toJSON = toJSONText

instance FromJSON DimensionType where
  parseJSON = parseJSONText "DimensionType"
