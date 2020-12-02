{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mp4FreeSpaceBox
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp4FreeSpaceBox where

import Network.AWS.Prelude

-- | Inserts a free-space box immediately after the moov box.
data Mp4FreeSpaceBox
  = MFSBExclude
  | MFSBInclude
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

instance FromText Mp4FreeSpaceBox where
  parser =
    takeLowerText >>= \case
      "exclude" -> pure MFSBExclude
      "include" -> pure MFSBInclude
      e ->
        fromTextError $
          "Failure parsing Mp4FreeSpaceBox from value: '" <> e
            <> "'. Accepted values: exclude, include"

instance ToText Mp4FreeSpaceBox where
  toText = \case
    MFSBExclude -> "EXCLUDE"
    MFSBInclude -> "INCLUDE"

instance Hashable Mp4FreeSpaceBox

instance NFData Mp4FreeSpaceBox

instance ToByteString Mp4FreeSpaceBox

instance ToQuery Mp4FreeSpaceBox

instance ToHeader Mp4FreeSpaceBox

instance ToJSON Mp4FreeSpaceBox where
  toJSON = toJSONText

instance FromJSON Mp4FreeSpaceBox where
  parseJSON = parseJSONText "Mp4FreeSpaceBox"
