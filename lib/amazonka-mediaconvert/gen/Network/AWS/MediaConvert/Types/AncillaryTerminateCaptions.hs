{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AncillaryTerminateCaptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AncillaryTerminateCaptions where

import Network.AWS.Prelude

-- | By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
data AncillaryTerminateCaptions
  = ATCDisabled
  | ATCEndOfInput
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

instance FromText AncillaryTerminateCaptions where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure ATCDisabled
      "end_of_input" -> pure ATCEndOfInput
      e ->
        fromTextError $
          "Failure parsing AncillaryTerminateCaptions from value: '" <> e
            <> "'. Accepted values: disabled, end_of_input"

instance ToText AncillaryTerminateCaptions where
  toText = \case
    ATCDisabled -> "DISABLED"
    ATCEndOfInput -> "END_OF_INPUT"

instance Hashable AncillaryTerminateCaptions

instance NFData AncillaryTerminateCaptions

instance ToByteString AncillaryTerminateCaptions

instance ToQuery AncillaryTerminateCaptions

instance ToHeader AncillaryTerminateCaptions

instance ToJSON AncillaryTerminateCaptions where
  toJSON = toJSONText

instance FromJSON AncillaryTerminateCaptions where
  parseJSON = parseJSONText "AncillaryTerminateCaptions"
