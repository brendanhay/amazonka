{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AacRateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AacRateControlMode where

import Network.AWS.Prelude

-- | Aac Rate Control Mode
data AacRateControlMode
  = ARCMCbr
  | ARCMVbr
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

instance FromText AacRateControlMode where
  parser =
    takeLowerText >>= \case
      "cbr" -> pure ARCMCbr
      "vbr" -> pure ARCMVbr
      e ->
        fromTextError $
          "Failure parsing AacRateControlMode from value: '" <> e
            <> "'. Accepted values: cbr, vbr"

instance ToText AacRateControlMode where
  toText = \case
    ARCMCbr -> "CBR"
    ARCMVbr -> "VBR"

instance Hashable AacRateControlMode

instance NFData AacRateControlMode

instance ToByteString AacRateControlMode

instance ToQuery AacRateControlMode

instance ToHeader AacRateControlMode

instance ToJSON AacRateControlMode where
  toJSON = toJSONText

instance FromJSON AacRateControlMode where
  parseJSON = parseJSONText "AacRateControlMode"
