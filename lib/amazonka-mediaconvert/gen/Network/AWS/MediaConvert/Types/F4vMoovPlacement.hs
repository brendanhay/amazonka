{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.F4vMoovPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.F4vMoovPlacement where

import Network.AWS.Prelude

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
data F4vMoovPlacement
  = FMPNormal
  | FMPProgressiveDownload
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

instance FromText F4vMoovPlacement where
  parser =
    takeLowerText >>= \case
      "normal" -> pure FMPNormal
      "progressive_download" -> pure FMPProgressiveDownload
      e ->
        fromTextError $
          "Failure parsing F4vMoovPlacement from value: '" <> e
            <> "'. Accepted values: normal, progressive_download"

instance ToText F4vMoovPlacement where
  toText = \case
    FMPNormal -> "NORMAL"
    FMPProgressiveDownload -> "PROGRESSIVE_DOWNLOAD"

instance Hashable F4vMoovPlacement

instance NFData F4vMoovPlacement

instance ToByteString F4vMoovPlacement

instance ToQuery F4vMoovPlacement

instance ToHeader F4vMoovPlacement

instance ToJSON F4vMoovPlacement where
  toJSON = toJSONText

instance FromJSON F4vMoovPlacement where
  parseJSON = parseJSONText "F4vMoovPlacement"
