{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Ac3MetadataControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Ac3MetadataControl where

import Network.AWS.Prelude

-- | Ac3 Metadata Control
data Ac3MetadataControl
  = AMCFollowInput
  | AMCUseConfigured
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

instance FromText Ac3MetadataControl where
  parser =
    takeLowerText >>= \case
      "follow_input" -> pure AMCFollowInput
      "use_configured" -> pure AMCUseConfigured
      e ->
        fromTextError $
          "Failure parsing Ac3MetadataControl from value: '" <> e
            <> "'. Accepted values: follow_input, use_configured"

instance ToText Ac3MetadataControl where
  toText = \case
    AMCFollowInput -> "FOLLOW_INPUT"
    AMCUseConfigured -> "USE_CONFIGURED"

instance Hashable Ac3MetadataControl

instance NFData Ac3MetadataControl

instance ToByteString Ac3MetadataControl

instance ToQuery Ac3MetadataControl

instance ToHeader Ac3MetadataControl

instance ToJSON Ac3MetadataControl where
  toJSON = toJSONText

instance FromJSON Ac3MetadataControl where
  parseJSON = parseJSONText "Ac3MetadataControl"
