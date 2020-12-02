{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3PassthroughControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3PassthroughControl where

import Network.AWS.Prelude

-- | When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it is present on the input. this detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
data Eac3PassthroughControl
  = NoPassthrough
  | WhenPossible
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

instance FromText Eac3PassthroughControl where
  parser =
    takeLowerText >>= \case
      "no_passthrough" -> pure NoPassthrough
      "when_possible" -> pure WhenPossible
      e ->
        fromTextError $
          "Failure parsing Eac3PassthroughControl from value: '" <> e
            <> "'. Accepted values: no_passthrough, when_possible"

instance ToText Eac3PassthroughControl where
  toText = \case
    NoPassthrough -> "NO_PASSTHROUGH"
    WhenPossible -> "WHEN_POSSIBLE"

instance Hashable Eac3PassthroughControl

instance NFData Eac3PassthroughControl

instance ToByteString Eac3PassthroughControl

instance ToQuery Eac3PassthroughControl

instance ToHeader Eac3PassthroughControl

instance ToJSON Eac3PassthroughControl where
  toJSON = toJSONText

instance FromJSON Eac3PassthroughControl where
  parseJSON = parseJSONText "Eac3PassthroughControl"
