{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264FieldEncoding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264FieldEncoding where

import Network.AWS.Prelude

-- | Keep the default value, PAFF, to have MediaConvert use PAFF encoding for interlaced outputs. Choose Force field (FORCE_FIELD) to disable PAFF encoding and create separate interlaced fields.
data H264FieldEncoding
  = ForceField
  | Paff
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

instance FromText H264FieldEncoding where
  parser =
    takeLowerText >>= \case
      "force_field" -> pure ForceField
      "paff" -> pure Paff
      e ->
        fromTextError $
          "Failure parsing H264FieldEncoding from value: '" <> e
            <> "'. Accepted values: force_field, paff"

instance ToText H264FieldEncoding where
  toText = \case
    ForceField -> "FORCE_FIELD"
    Paff -> "PAFF"

instance Hashable H264FieldEncoding

instance NFData H264FieldEncoding

instance ToByteString H264FieldEncoding

instance ToQuery H264FieldEncoding

instance ToHeader H264FieldEncoding

instance ToJSON H264FieldEncoding where
  toJSON = toJSONText

instance FromJSON H264FieldEncoding where
  parseJSON = parseJSONText "H264FieldEncoding"
