{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ChannelClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ChannelClass where

import Network.AWS.Prelude

-- | A standard channel has two encoding pipelines and a single pipeline channel only has one.
data ChannelClass
  = CCSinglePipeline
  | CCStandard
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

instance FromText ChannelClass where
  parser =
    takeLowerText >>= \case
      "single_pipeline" -> pure CCSinglePipeline
      "standard" -> pure CCStandard
      e ->
        fromTextError $
          "Failure parsing ChannelClass from value: '" <> e
            <> "'. Accepted values: single_pipeline, standard"

instance ToText ChannelClass where
  toText = \case
    CCSinglePipeline -> "SINGLE_PIPELINE"
    CCStandard -> "STANDARD"

instance Hashable ChannelClass

instance NFData ChannelClass

instance ToByteString ChannelClass

instance ToQuery ChannelClass

instance ToHeader ChannelClass

instance ToJSON ChannelClass where
  toJSON = toJSONText

instance FromJSON ChannelClass where
  parseJSON = parseJSONText "ChannelClass"
