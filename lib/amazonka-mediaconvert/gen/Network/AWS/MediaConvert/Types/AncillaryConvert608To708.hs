{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AncillaryConvert608To708
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AncillaryConvert608To708 where

import Network.AWS.Prelude

-- | Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
data AncillaryConvert608To708
  = ACTDisabled
  | ACTUpconvert
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

instance FromText AncillaryConvert608To708 where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure ACTDisabled
      "upconvert" -> pure ACTUpconvert
      e ->
        fromTextError $
          "Failure parsing AncillaryConvert608To708 from value: '" <> e
            <> "'. Accepted values: disabled, upconvert"

instance ToText AncillaryConvert608To708 where
  toText = \case
    ACTDisabled -> "DISABLED"
    ACTUpconvert -> "UPCONVERT"

instance Hashable AncillaryConvert608To708

instance NFData AncillaryConvert608To708

instance ToByteString AncillaryConvert608To708

instance ToQuery AncillaryConvert608To708

instance ToHeader AncillaryConvert608To708

instance ToJSON AncillaryConvert608To708 where
  toJSON = toJSONText

instance FromJSON AncillaryConvert608To708 where
  parseJSON = parseJSONText "AncillaryConvert608To708"
