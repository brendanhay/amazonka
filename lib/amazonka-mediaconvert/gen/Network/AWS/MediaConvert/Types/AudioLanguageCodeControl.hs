{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioLanguageCodeControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioLanguageCodeControl where

import Network.AWS.Prelude

-- | Specify which source for language code takes precedence for this audio track. When you choose Follow input (FOLLOW_INPUT), the service uses the language code from the input track if it's present. If there's no languge code on the input track, the service uses the code that you specify in the setting Language code (languageCode or customLanguageCode). When you choose Use configured (USE_CONFIGURED), the service uses the language code that you specify.
data AudioLanguageCodeControl
  = FollowInput
  | UseConfigured
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

instance FromText AudioLanguageCodeControl where
  parser =
    takeLowerText >>= \case
      "follow_input" -> pure FollowInput
      "use_configured" -> pure UseConfigured
      e ->
        fromTextError $
          "Failure parsing AudioLanguageCodeControl from value: '" <> e
            <> "'. Accepted values: follow_input, use_configured"

instance ToText AudioLanguageCodeControl where
  toText = \case
    FollowInput -> "FOLLOW_INPUT"
    UseConfigured -> "USE_CONFIGURED"

instance Hashable AudioLanguageCodeControl

instance NFData AudioLanguageCodeControl

instance ToByteString AudioLanguageCodeControl

instance ToQuery AudioLanguageCodeControl

instance ToHeader AudioLanguageCodeControl

instance ToJSON AudioLanguageCodeControl where
  toJSON = toJSONText

instance FromJSON AudioLanguageCodeControl where
  parseJSON = parseJSONText "AudioLanguageCodeControl"
