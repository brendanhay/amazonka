{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264TemporalAq
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264TemporalAq where

import Network.AWS.Prelude

-- | H264 Temporal Aq
data H264TemporalAq
  = HTADisabled
  | HTAEnabled
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

instance FromText H264TemporalAq where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HTADisabled
      "enabled" -> pure HTAEnabled
      e ->
        fromTextError $
          "Failure parsing H264TemporalAq from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText H264TemporalAq where
  toText = \case
    HTADisabled -> "DISABLED"
    HTAEnabled -> "ENABLED"

instance Hashable H264TemporalAq

instance NFData H264TemporalAq

instance ToByteString H264TemporalAq

instance ToQuery H264TemporalAq

instance ToHeader H264TemporalAq

instance ToJSON H264TemporalAq where
  toJSON = toJSONText

instance FromJSON H264TemporalAq where
  parseJSON = parseJSONText "H264TemporalAq"
