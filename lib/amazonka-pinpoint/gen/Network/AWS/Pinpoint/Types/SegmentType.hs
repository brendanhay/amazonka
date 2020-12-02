{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentType where

import Network.AWS.Prelude

data SegmentType
  = Dimensional
  | Import
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

instance FromText SegmentType where
  parser =
    takeLowerText >>= \case
      "dimensional" -> pure Dimensional
      "import" -> pure Import
      e ->
        fromTextError $
          "Failure parsing SegmentType from value: '" <> e
            <> "'. Accepted values: dimensional, import"

instance ToText SegmentType where
  toText = \case
    Dimensional -> "DIMENSIONAL"
    Import -> "IMPORT"

instance Hashable SegmentType

instance NFData SegmentType

instance ToByteString SegmentType

instance ToQuery SegmentType

instance ToHeader SegmentType

instance FromJSON SegmentType where
  parseJSON = parseJSONText "SegmentType"
