{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioDefaultSelection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioDefaultSelection where

import Network.AWS.Prelude

-- | Enable this setting on one audio selector to set it as the default for the job. The service uses this default for outputs where it can't find the specified input audio. If you don't set a default, those outputs have no audio.
data AudioDefaultSelection
  = ADSDefault
  | ADSNotDefault
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

instance FromText AudioDefaultSelection where
  parser =
    takeLowerText >>= \case
      "default" -> pure ADSDefault
      "not_default" -> pure ADSNotDefault
      e ->
        fromTextError $
          "Failure parsing AudioDefaultSelection from value: '" <> e
            <> "'. Accepted values: default, not_default"

instance ToText AudioDefaultSelection where
  toText = \case
    ADSDefault -> "DEFAULT"
    ADSNotDefault -> "NOT_DEFAULT"

instance Hashable AudioDefaultSelection

instance NFData AudioDefaultSelection

instance ToByteString AudioDefaultSelection

instance ToQuery AudioDefaultSelection

instance ToHeader AudioDefaultSelection

instance ToJSON AudioDefaultSelection where
  toJSON = toJSONText

instance FromJSON AudioDefaultSelection where
  parseJSON = parseJSONText "AudioDefaultSelection"
