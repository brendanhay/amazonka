{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264RepeatPps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264RepeatPps where

import Network.AWS.Prelude

-- | Places a PPS header on each encoded picture, even if repeated.
data H264RepeatPps
  = HRPDisabled
  | HRPEnabled
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

instance FromText H264RepeatPps where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HRPDisabled
      "enabled" -> pure HRPEnabled
      e ->
        fromTextError $
          "Failure parsing H264RepeatPps from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText H264RepeatPps where
  toText = \case
    HRPDisabled -> "DISABLED"
    HRPEnabled -> "ENABLED"

instance Hashable H264RepeatPps

instance NFData H264RepeatPps

instance ToByteString H264RepeatPps

instance ToQuery H264RepeatPps

instance ToHeader H264RepeatPps

instance ToJSON H264RepeatPps where
  toJSON = toJSONText

instance FromJSON H264RepeatPps where
  parseJSON = parseJSONText "H264RepeatPps"
