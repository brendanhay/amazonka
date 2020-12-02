{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264TimecodeInsertionBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264TimecodeInsertionBehavior where

import Network.AWS.Prelude

-- | H264 Timecode Insertion Behavior
data H264TimecodeInsertionBehavior
  = H26Disabled
  | H26PicTimingSei
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

instance FromText H264TimecodeInsertionBehavior where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure H26Disabled
      "pic_timing_sei" -> pure H26PicTimingSei
      e ->
        fromTextError $
          "Failure parsing H264TimecodeInsertionBehavior from value: '" <> e
            <> "'. Accepted values: disabled, pic_timing_sei"

instance ToText H264TimecodeInsertionBehavior where
  toText = \case
    H26Disabled -> "DISABLED"
    H26PicTimingSei -> "PIC_TIMING_SEI"

instance Hashable H264TimecodeInsertionBehavior

instance NFData H264TimecodeInsertionBehavior

instance ToByteString H264TimecodeInsertionBehavior

instance ToQuery H264TimecodeInsertionBehavior

instance ToHeader H264TimecodeInsertionBehavior

instance ToJSON H264TimecodeInsertionBehavior where
  toJSON = toJSONText

instance FromJSON H264TimecodeInsertionBehavior where
  parseJSON = parseJSONText "H264TimecodeInsertionBehavior"
