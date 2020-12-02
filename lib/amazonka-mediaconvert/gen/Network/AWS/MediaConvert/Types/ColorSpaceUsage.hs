{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ColorSpaceUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ColorSpaceUsage where

import Network.AWS.Prelude

-- | There are two sources for color metadata, the input file and the job input settings Color space (ColorSpace) and HDR master display information settings(Hdr10Metadata). The Color space usage setting determines which takes precedence. Choose Force (FORCE) to use color metadata from the input job settings. If you don't specify values for those settings, the service defaults to using metadata from your input. FALLBACK - Choose Fallback (FALLBACK) to use color metadata from the source when it is present. If there's no color metadata in your input file, the service defaults to using values you specify in the input settings.
data ColorSpaceUsage
  = Fallback
  | Force
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

instance FromText ColorSpaceUsage where
  parser =
    takeLowerText >>= \case
      "fallback" -> pure Fallback
      "force" -> pure Force
      e ->
        fromTextError $
          "Failure parsing ColorSpaceUsage from value: '" <> e
            <> "'. Accepted values: fallback, force"

instance ToText ColorSpaceUsage where
  toText = \case
    Fallback -> "FALLBACK"
    Force -> "FORCE"

instance Hashable ColorSpaceUsage

instance NFData ColorSpaceUsage

instance ToByteString ColorSpaceUsage

instance ToQuery ColorSpaceUsage

instance ToHeader ColorSpaceUsage

instance ToJSON ColorSpaceUsage where
  toJSON = toJSONText

instance FromJSON ColorSpaceUsage where
  parseJSON = parseJSONText "ColorSpaceUsage"
