{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.DASHPlaybackMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.DASHPlaybackMode where

import Network.AWS.Prelude

data DASHPlaybackMode
  = DASHPMLive
  | DASHPMLiveReplay
  | DASHPMOnDemand
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

instance FromText DASHPlaybackMode where
  parser =
    takeLowerText >>= \case
      "live" -> pure DASHPMLive
      "live_replay" -> pure DASHPMLiveReplay
      "on_demand" -> pure DASHPMOnDemand
      e ->
        fromTextError $
          "Failure parsing DASHPlaybackMode from value: '" <> e
            <> "'. Accepted values: live, live_replay, on_demand"

instance ToText DASHPlaybackMode where
  toText = \case
    DASHPMLive -> "LIVE"
    DASHPMLiveReplay -> "LIVE_REPLAY"
    DASHPMOnDemand -> "ON_DEMAND"

instance Hashable DASHPlaybackMode

instance NFData DASHPlaybackMode

instance ToByteString DASHPlaybackMode

instance ToQuery DASHPlaybackMode

instance ToHeader DASHPlaybackMode

instance ToJSON DASHPlaybackMode where
  toJSON = toJSONText
