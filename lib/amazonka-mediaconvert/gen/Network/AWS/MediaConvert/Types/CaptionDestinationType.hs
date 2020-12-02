{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CaptionDestinationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CaptionDestinationType where

import Network.AWS.Prelude

-- | Specify the format for this set of captions on this output. The default format is embedded without SCTE-20. Other options are embedded with SCTE-20, burn-in, DVB-sub, IMSC, SCC, SRT, teletext, TTML, and web-VTT. If you are using SCTE-20, choose SCTE-20 plus embedded (SCTE20_PLUS_EMBEDDED) to create an output that complies with the SCTE-43 spec. To create a non-compliant output where the embedded captions come first, choose Embedded plus SCTE-20 (EMBEDDED_PLUS_SCTE20).
data CaptionDestinationType
  = BurnIn
  | DvbSub
  | Embedded
  | EmbeddedPlusSCTE20
  | Imsc
  | SCTE20PlusEmbedded
  | Scc
  | Smi
  | Srt
  | Teletext
  | Ttml
  | Webvtt
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

instance FromText CaptionDestinationType where
  parser =
    takeLowerText >>= \case
      "burn_in" -> pure BurnIn
      "dvb_sub" -> pure DvbSub
      "embedded" -> pure Embedded
      "embedded_plus_scte20" -> pure EmbeddedPlusSCTE20
      "imsc" -> pure Imsc
      "scte20_plus_embedded" -> pure SCTE20PlusEmbedded
      "scc" -> pure Scc
      "smi" -> pure Smi
      "srt" -> pure Srt
      "teletext" -> pure Teletext
      "ttml" -> pure Ttml
      "webvtt" -> pure Webvtt
      e ->
        fromTextError $
          "Failure parsing CaptionDestinationType from value: '" <> e
            <> "'. Accepted values: burn_in, dvb_sub, embedded, embedded_plus_scte20, imsc, scte20_plus_embedded, scc, smi, srt, teletext, ttml, webvtt"

instance ToText CaptionDestinationType where
  toText = \case
    BurnIn -> "BURN_IN"
    DvbSub -> "DVB_SUB"
    Embedded -> "EMBEDDED"
    EmbeddedPlusSCTE20 -> "EMBEDDED_PLUS_SCTE20"
    Imsc -> "IMSC"
    SCTE20PlusEmbedded -> "SCTE20_PLUS_EMBEDDED"
    Scc -> "SCC"
    Smi -> "SMI"
    Srt -> "SRT"
    Teletext -> "TELETEXT"
    Ttml -> "TTML"
    Webvtt -> "WEBVTT"

instance Hashable CaptionDestinationType

instance NFData CaptionDestinationType

instance ToByteString CaptionDestinationType

instance ToQuery CaptionDestinationType

instance ToHeader CaptionDestinationType

instance ToJSON CaptionDestinationType where
  toJSON = toJSONText

instance FromJSON CaptionDestinationType where
  parseJSON = parseJSONText "CaptionDestinationType"
