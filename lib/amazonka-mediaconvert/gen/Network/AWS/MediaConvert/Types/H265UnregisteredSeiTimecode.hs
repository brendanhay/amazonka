{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265UnregisteredSeiTimecode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265UnregisteredSeiTimecode where

import Network.AWS.Prelude

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
data H265UnregisteredSeiTimecode
  = HUSTDisabled
  | HUSTEnabled
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

instance FromText H265UnregisteredSeiTimecode where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HUSTDisabled
      "enabled" -> pure HUSTEnabled
      e ->
        fromTextError $
          "Failure parsing H265UnregisteredSeiTimecode from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText H265UnregisteredSeiTimecode where
  toText = \case
    HUSTDisabled -> "DISABLED"
    HUSTEnabled -> "ENABLED"

instance Hashable H265UnregisteredSeiTimecode

instance NFData H265UnregisteredSeiTimecode

instance ToByteString H265UnregisteredSeiTimecode

instance ToQuery H265UnregisteredSeiTimecode

instance ToHeader H265UnregisteredSeiTimecode

instance ToJSON H265UnregisteredSeiTimecode where
  toJSON = toJSONText

instance FromJSON H265UnregisteredSeiTimecode where
  parseJSON = parseJSONText "H265UnregisteredSeiTimecode"
