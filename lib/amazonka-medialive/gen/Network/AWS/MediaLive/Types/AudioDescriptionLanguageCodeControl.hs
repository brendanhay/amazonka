{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioDescriptionLanguageCodeControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioDescriptionLanguageCodeControl where

import Network.AWS.Prelude

-- | Audio Description Language Code Control
data AudioDescriptionLanguageCodeControl
  = ADLCCFollowInput
  | ADLCCUseConfigured
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

instance FromText AudioDescriptionLanguageCodeControl where
  parser =
    takeLowerText >>= \case
      "follow_input" -> pure ADLCCFollowInput
      "use_configured" -> pure ADLCCUseConfigured
      e ->
        fromTextError $
          "Failure parsing AudioDescriptionLanguageCodeControl from value: '" <> e
            <> "'. Accepted values: follow_input, use_configured"

instance ToText AudioDescriptionLanguageCodeControl where
  toText = \case
    ADLCCFollowInput -> "FOLLOW_INPUT"
    ADLCCUseConfigured -> "USE_CONFIGURED"

instance Hashable AudioDescriptionLanguageCodeControl

instance NFData AudioDescriptionLanguageCodeControl

instance ToByteString AudioDescriptionLanguageCodeControl

instance ToQuery AudioDescriptionLanguageCodeControl

instance ToHeader AudioDescriptionLanguageCodeControl

instance ToJSON AudioDescriptionLanguageCodeControl where
  toJSON = toJSONText

instance FromJSON AudioDescriptionLanguageCodeControl where
  parseJSON = parseJSONText "AudioDescriptionLanguageCodeControl"
