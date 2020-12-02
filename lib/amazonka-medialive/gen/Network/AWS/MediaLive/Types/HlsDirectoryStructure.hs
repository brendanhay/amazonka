{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsDirectoryStructure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsDirectoryStructure where

import Network.AWS.Prelude

-- | Hls Directory Structure
data HlsDirectoryStructure
  = SingleDirectory
  | SubdirectoryPerStream
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

instance FromText HlsDirectoryStructure where
  parser =
    takeLowerText >>= \case
      "single_directory" -> pure SingleDirectory
      "subdirectory_per_stream" -> pure SubdirectoryPerStream
      e ->
        fromTextError $
          "Failure parsing HlsDirectoryStructure from value: '" <> e
            <> "'. Accepted values: single_directory, subdirectory_per_stream"

instance ToText HlsDirectoryStructure where
  toText = \case
    SingleDirectory -> "SINGLE_DIRECTORY"
    SubdirectoryPerStream -> "SUBDIRECTORY_PER_STREAM"

instance Hashable HlsDirectoryStructure

instance NFData HlsDirectoryStructure

instance ToByteString HlsDirectoryStructure

instance ToQuery HlsDirectoryStructure

instance ToHeader HlsDirectoryStructure

instance ToJSON HlsDirectoryStructure where
  toJSON = toJSONText

instance FromJSON HlsDirectoryStructure where
  parseJSON = parseJSONText "HlsDirectoryStructure"
