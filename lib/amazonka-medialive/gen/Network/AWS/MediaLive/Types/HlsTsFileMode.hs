{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsTsFileMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsTsFileMode where

import Network.AWS.Prelude

-- | Hls Ts File Mode
data HlsTsFileMode
  = SegmentedFiles
  | SingleFile
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

instance FromText HlsTsFileMode where
  parser =
    takeLowerText >>= \case
      "segmented_files" -> pure SegmentedFiles
      "single_file" -> pure SingleFile
      e ->
        fromTextError $
          "Failure parsing HlsTsFileMode from value: '" <> e
            <> "'. Accepted values: segmented_files, single_file"

instance ToText HlsTsFileMode where
  toText = \case
    SegmentedFiles -> "SEGMENTED_FILES"
    SingleFile -> "SINGLE_FILE"

instance Hashable HlsTsFileMode

instance NFData HlsTsFileMode

instance ToByteString HlsTsFileMode

instance ToQuery HlsTsFileMode

instance ToHeader HlsTsFileMode

instance ToJSON HlsTsFileMode where
  toJSON = toJSONText

instance FromJSON HlsTsFileMode where
  parseJSON = parseJSONText "HlsTsFileMode"
