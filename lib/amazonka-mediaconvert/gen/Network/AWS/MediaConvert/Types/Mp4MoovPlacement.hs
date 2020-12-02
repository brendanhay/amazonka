{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mp4MoovPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp4MoovPlacement where

import Network.AWS.Prelude

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
data Mp4MoovPlacement
  = MMPNormal
  | MMPProgressiveDownload
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

instance FromText Mp4MoovPlacement where
  parser =
    takeLowerText >>= \case
      "normal" -> pure MMPNormal
      "progressive_download" -> pure MMPProgressiveDownload
      e ->
        fromTextError $
          "Failure parsing Mp4MoovPlacement from value: '" <> e
            <> "'. Accepted values: normal, progressive_download"

instance ToText Mp4MoovPlacement where
  toText = \case
    MMPNormal -> "NORMAL"
    MMPProgressiveDownload -> "PROGRESSIVE_DOWNLOAD"

instance Hashable Mp4MoovPlacement

instance NFData Mp4MoovPlacement

instance ToByteString Mp4MoovPlacement

instance ToQuery Mp4MoovPlacement

instance ToHeader Mp4MoovPlacement

instance ToJSON Mp4MoovPlacement where
  toJSON = toJSONText

instance FromJSON Mp4MoovPlacement where
  parseJSON = parseJSONText "Mp4MoovPlacement"
