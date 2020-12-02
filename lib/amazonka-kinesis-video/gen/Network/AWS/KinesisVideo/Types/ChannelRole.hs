{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.ChannelRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.ChannelRole where

import Network.AWS.Prelude

data ChannelRole
  = Master
  | Viewer
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

instance FromText ChannelRole where
  parser =
    takeLowerText >>= \case
      "master" -> pure Master
      "viewer" -> pure Viewer
      e ->
        fromTextError $
          "Failure parsing ChannelRole from value: '" <> e
            <> "'. Accepted values: master, viewer"

instance ToText ChannelRole where
  toText = \case
    Master -> "MASTER"
    Viewer -> "VIEWER"

instance Hashable ChannelRole

instance NFData ChannelRole

instance ToByteString ChannelRole

instance ToQuery ChannelRole

instance ToHeader ChannelRole

instance ToJSON ChannelRole where
  toJSON = toJSONText
