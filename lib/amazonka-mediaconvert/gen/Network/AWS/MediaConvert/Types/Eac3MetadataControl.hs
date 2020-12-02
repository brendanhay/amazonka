{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3MetadataControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3MetadataControl where

import Network.AWS.Prelude

-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
data Eac3MetadataControl
  = EMCFollowInput
  | EMCUseConfigured
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

instance FromText Eac3MetadataControl where
  parser =
    takeLowerText >>= \case
      "follow_input" -> pure EMCFollowInput
      "use_configured" -> pure EMCUseConfigured
      e ->
        fromTextError $
          "Failure parsing Eac3MetadataControl from value: '" <> e
            <> "'. Accepted values: follow_input, use_configured"

instance ToText Eac3MetadataControl where
  toText = \case
    EMCFollowInput -> "FOLLOW_INPUT"
    EMCUseConfigured -> "USE_CONFIGURED"

instance Hashable Eac3MetadataControl

instance NFData Eac3MetadataControl

instance ToByteString Eac3MetadataControl

instance ToQuery Eac3MetadataControl

instance ToHeader Eac3MetadataControl

instance ToJSON Eac3MetadataControl where
  toJSON = toJSONText

instance FromJSON Eac3MetadataControl where
  parseJSON = parseJSONText "Eac3MetadataControl"
