{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265RateControlMode where

import Network.AWS.Prelude

-- | Use this setting to specify whether this output has a variable bitrate (VBR), constant bitrate (CBR) or quality-defined variable bitrate (QVBR).
data H265RateControlMode
  = HRCMCbr
  | HRCMQvbr
  | HRCMVbr
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

instance FromText H265RateControlMode where
  parser =
    takeLowerText >>= \case
      "cbr" -> pure HRCMCbr
      "qvbr" -> pure HRCMQvbr
      "vbr" -> pure HRCMVbr
      e ->
        fromTextError $
          "Failure parsing H265RateControlMode from value: '" <> e
            <> "'. Accepted values: cbr, qvbr, vbr"

instance ToText H265RateControlMode where
  toText = \case
    HRCMCbr -> "CBR"
    HRCMQvbr -> "QVBR"
    HRCMVbr -> "VBR"

instance Hashable H265RateControlMode

instance NFData H265RateControlMode

instance ToByteString H265RateControlMode

instance ToQuery H265RateControlMode

instance ToHeader H265RateControlMode

instance ToJSON H265RateControlMode where
  toJSON = toJSONText

instance FromJSON H265RateControlMode where
  parseJSON = parseJSONText "H265RateControlMode"
