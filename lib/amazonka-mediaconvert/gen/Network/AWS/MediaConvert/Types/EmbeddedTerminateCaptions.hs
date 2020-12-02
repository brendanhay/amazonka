{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.EmbeddedTerminateCaptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.EmbeddedTerminateCaptions where

import Network.AWS.Prelude

-- | By default, the service terminates any unterminated captions at the end of each input. If you want the caption to continue onto your next input, disable this setting.
data EmbeddedTerminateCaptions
  = ETCDisabled
  | ETCEndOfInput
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

instance FromText EmbeddedTerminateCaptions where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure ETCDisabled
      "end_of_input" -> pure ETCEndOfInput
      e ->
        fromTextError $
          "Failure parsing EmbeddedTerminateCaptions from value: '" <> e
            <> "'. Accepted values: disabled, end_of_input"

instance ToText EmbeddedTerminateCaptions where
  toText = \case
    ETCDisabled -> "DISABLED"
    ETCEndOfInput -> "END_OF_INPUT"

instance Hashable EmbeddedTerminateCaptions

instance NFData EmbeddedTerminateCaptions

instance ToByteString EmbeddedTerminateCaptions

instance ToQuery EmbeddedTerminateCaptions

instance ToHeader EmbeddedTerminateCaptions

instance ToJSON EmbeddedTerminateCaptions where
  toJSON = toJSONText

instance FromJSON EmbeddedTerminateCaptions where
  parseJSON = parseJSONText "EmbeddedTerminateCaptions"
