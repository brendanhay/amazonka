{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Av1RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Av1RateControlMode where

import Network.AWS.Prelude

-- | 'With AV1 outputs, for rate control mode, MediaConvert supports only quality-defined variable bitrate (QVBR). You can''t use CBR or VBR.'
data Av1RateControlMode = Qvbr
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

instance FromText Av1RateControlMode where
  parser =
    takeLowerText >>= \case
      "qvbr" -> pure Qvbr
      e ->
        fromTextError $
          "Failure parsing Av1RateControlMode from value: '" <> e
            <> "'. Accepted values: qvbr"

instance ToText Av1RateControlMode where
  toText = \case
    Qvbr -> "QVBR"

instance Hashable Av1RateControlMode

instance NFData Av1RateControlMode

instance ToByteString Av1RateControlMode

instance ToQuery Av1RateControlMode

instance ToHeader Av1RateControlMode

instance ToJSON Av1RateControlMode where
  toJSON = toJSONText

instance FromJSON Av1RateControlMode where
  parseJSON = parseJSONText "Av1RateControlMode"
