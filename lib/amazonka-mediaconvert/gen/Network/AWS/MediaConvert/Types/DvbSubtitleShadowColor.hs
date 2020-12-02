{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleShadowColor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitleShadowColor where

import Network.AWS.Prelude

-- | Specifies the color of the shadow cast by the captions.
--
-- All burn-in and DVB-Sub font settings must match.
data DvbSubtitleShadowColor
  = DSSCBlack
  | DSSCNone
  | DSSCWhite
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

instance FromText DvbSubtitleShadowColor where
  parser =
    takeLowerText >>= \case
      "black" -> pure DSSCBlack
      "none" -> pure DSSCNone
      "white" -> pure DSSCWhite
      e ->
        fromTextError $
          "Failure parsing DvbSubtitleShadowColor from value: '" <> e
            <> "'. Accepted values: black, none, white"

instance ToText DvbSubtitleShadowColor where
  toText = \case
    DSSCBlack -> "BLACK"
    DSSCNone -> "NONE"
    DSSCWhite -> "WHITE"

instance Hashable DvbSubtitleShadowColor

instance NFData DvbSubtitleShadowColor

instance ToByteString DvbSubtitleShadowColor

instance ToQuery DvbSubtitleShadowColor

instance ToHeader DvbSubtitleShadowColor

instance ToJSON DvbSubtitleShadowColor where
  toJSON = toJSONText

instance FromJSON DvbSubtitleShadowColor where
  parseJSON = parseJSONText "DvbSubtitleShadowColor"
