{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.APICacheType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.APICacheType where

import Network.AWS.Prelude

data APICacheType
  = Large
  | Large12X
  | Large2X
  | Large4X
  | Large8X
  | Medium
  | R42XLARGE
  | R44XLARGE
  | R48XLARGE
  | R4Large
  | R4XLarge
  | Small
  | T2Medium
  | T2Small
  | XLarge
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

instance FromText APICacheType where
  parser =
    takeLowerText >>= \case
      "large" -> pure Large
      "large_12x" -> pure Large12X
      "large_2x" -> pure Large2X
      "large_4x" -> pure Large4X
      "large_8x" -> pure Large8X
      "medium" -> pure Medium
      "r4_2xlarge" -> pure R42XLARGE
      "r4_4xlarge" -> pure R44XLARGE
      "r4_8xlarge" -> pure R48XLARGE
      "r4_large" -> pure R4Large
      "r4_xlarge" -> pure R4XLarge
      "small" -> pure Small
      "t2_medium" -> pure T2Medium
      "t2_small" -> pure T2Small
      "xlarge" -> pure XLarge
      e ->
        fromTextError $
          "Failure parsing APICacheType from value: '" <> e
            <> "'. Accepted values: large, large_12x, large_2x, large_4x, large_8x, medium, r4_2xlarge, r4_4xlarge, r4_8xlarge, r4_large, r4_xlarge, small, t2_medium, t2_small, xlarge"

instance ToText APICacheType where
  toText = \case
    Large -> "LARGE"
    Large12X -> "LARGE_12X"
    Large2X -> "LARGE_2X"
    Large4X -> "LARGE_4X"
    Large8X -> "LARGE_8X"
    Medium -> "MEDIUM"
    R42XLARGE -> "R4_2XLARGE"
    R44XLARGE -> "R4_4XLARGE"
    R48XLARGE -> "R4_8XLARGE"
    R4Large -> "R4_LARGE"
    R4XLarge -> "R4_XLARGE"
    Small -> "SMALL"
    T2Medium -> "T2_MEDIUM"
    T2Small -> "T2_SMALL"
    XLarge -> "XLARGE"

instance Hashable APICacheType

instance NFData APICacheType

instance ToByteString APICacheType

instance ToQuery APICacheType

instance ToHeader APICacheType

instance ToJSON APICacheType where
  toJSON = toJSONText

instance FromJSON APICacheType where
  parseJSON = parseJSONText "APICacheType"
