{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265TimecodeInsertionBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265TimecodeInsertionBehavior where

import Network.AWS.Prelude

-- | H265 Timecode Insertion Behavior
data H265TimecodeInsertionBehavior
  = HTIBDisabled
  | HTIBPicTimingSei
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

instance FromText H265TimecodeInsertionBehavior where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HTIBDisabled
      "pic_timing_sei" -> pure HTIBPicTimingSei
      e ->
        fromTextError $
          "Failure parsing H265TimecodeInsertionBehavior from value: '" <> e
            <> "'. Accepted values: disabled, pic_timing_sei"

instance ToText H265TimecodeInsertionBehavior where
  toText = \case
    HTIBDisabled -> "DISABLED"
    HTIBPicTimingSei -> "PIC_TIMING_SEI"

instance Hashable H265TimecodeInsertionBehavior

instance NFData H265TimecodeInsertionBehavior

instance ToByteString H265TimecodeInsertionBehavior

instance ToQuery H265TimecodeInsertionBehavior

instance ToHeader H265TimecodeInsertionBehavior

instance ToJSON H265TimecodeInsertionBehavior where
  toJSON = toJSONText

instance FromJSON H265TimecodeInsertionBehavior where
  parseJSON = parseJSONText "H265TimecodeInsertionBehavior"
