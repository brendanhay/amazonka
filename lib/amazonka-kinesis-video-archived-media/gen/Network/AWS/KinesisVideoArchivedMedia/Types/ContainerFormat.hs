{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.ContainerFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.ContainerFormat where

import Network.AWS.Prelude

data ContainerFormat
  = FragmentedMP4
  | MpegTs
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

instance FromText ContainerFormat where
  parser =
    takeLowerText >>= \case
      "fragmented_mp4" -> pure FragmentedMP4
      "mpeg_ts" -> pure MpegTs
      e ->
        fromTextError $
          "Failure parsing ContainerFormat from value: '" <> e
            <> "'. Accepted values: fragmented_mp4, mpeg_ts"

instance ToText ContainerFormat where
  toText = \case
    FragmentedMP4 -> "FRAGMENTED_MP4"
    MpegTs -> "MPEG_TS"

instance Hashable ContainerFormat

instance NFData ContainerFormat

instance ToByteString ContainerFormat

instance ToQuery ContainerFormat

instance ToHeader ContainerFormat

instance ToJSON ContainerFormat where
  toJSON = toJSONText
