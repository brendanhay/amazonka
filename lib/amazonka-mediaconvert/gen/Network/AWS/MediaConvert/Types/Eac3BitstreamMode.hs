{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3BitstreamMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3BitstreamMode where

import Network.AWS.Prelude

-- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
data Eac3BitstreamMode
  = EBMCommentary
  | EBMCompleteMain
  | EBMEmergency
  | EBMHearingImpaired
  | EBMVisuallyImpaired
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

instance FromText Eac3BitstreamMode where
  parser =
    takeLowerText >>= \case
      "commentary" -> pure EBMCommentary
      "complete_main" -> pure EBMCompleteMain
      "emergency" -> pure EBMEmergency
      "hearing_impaired" -> pure EBMHearingImpaired
      "visually_impaired" -> pure EBMVisuallyImpaired
      e ->
        fromTextError $
          "Failure parsing Eac3BitstreamMode from value: '" <> e
            <> "'. Accepted values: commentary, complete_main, emergency, hearing_impaired, visually_impaired"

instance ToText Eac3BitstreamMode where
  toText = \case
    EBMCommentary -> "COMMENTARY"
    EBMCompleteMain -> "COMPLETE_MAIN"
    EBMEmergency -> "EMERGENCY"
    EBMHearingImpaired -> "HEARING_IMPAIRED"
    EBMVisuallyImpaired -> "VISUALLY_IMPAIRED"

instance Hashable Eac3BitstreamMode

instance NFData Eac3BitstreamMode

instance ToByteString Eac3BitstreamMode

instance ToQuery Eac3BitstreamMode

instance ToHeader Eac3BitstreamMode

instance ToJSON Eac3BitstreamMode where
  toJSON = toJSONText

instance FromJSON Eac3BitstreamMode where
  parseJSON = parseJSONText "Eac3BitstreamMode"
