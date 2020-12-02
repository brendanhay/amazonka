{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmfcScte35Source
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmfcScte35Source where

import Network.AWS.Prelude

-- | Ignore this setting unless you have SCTE-35 markers in your input video file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want those SCTE-35 markers in this output.
data CmfcScte35Source
  = CSSNone
  | CSSPassthrough
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

instance FromText CmfcScte35Source where
  parser =
    takeLowerText >>= \case
      "none" -> pure CSSNone
      "passthrough" -> pure CSSPassthrough
      e ->
        fromTextError $
          "Failure parsing CmfcScte35Source from value: '" <> e
            <> "'. Accepted values: none, passthrough"

instance ToText CmfcScte35Source where
  toText = \case
    CSSNone -> "NONE"
    CSSPassthrough -> "PASSTHROUGH"

instance Hashable CmfcScte35Source

instance NFData CmfcScte35Source

instance ToByteString CmfcScte35Source

instance ToQuery CmfcScte35Source

instance ToHeader CmfcScte35Source

instance ToJSON CmfcScte35Source where
  toJSON = toJSONText

instance FromJSON CmfcScte35Source where
  parseJSON = parseJSONText "CmfcScte35Source"
