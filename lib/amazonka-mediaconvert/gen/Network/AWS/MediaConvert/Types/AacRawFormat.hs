{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacRawFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AacRawFormat where

import Network.AWS.Prelude

-- | Enables LATM/LOAS AAC output. Note that if you use LATM/LOAS AAC in an output, you must choose "No container" for the output container.
data AacRawFormat
  = ARFLatmLoas
  | ARFNone
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

instance FromText AacRawFormat where
  parser =
    takeLowerText >>= \case
      "latm_loas" -> pure ARFLatmLoas
      "none" -> pure ARFNone
      e ->
        fromTextError $
          "Failure parsing AacRawFormat from value: '" <> e
            <> "'. Accepted values: latm_loas, none"

instance ToText AacRawFormat where
  toText = \case
    ARFLatmLoas -> "LATM_LOAS"
    ARFNone -> "NONE"

instance Hashable AacRawFormat

instance NFData AacRawFormat

instance ToByteString AacRawFormat

instance ToQuery AacRawFormat

instance ToHeader AacRawFormat

instance ToJSON AacRawFormat where
  toJSON = toJSONText

instance FromJSON AacRawFormat where
  parseJSON = parseJSONText "AacRawFormat"
