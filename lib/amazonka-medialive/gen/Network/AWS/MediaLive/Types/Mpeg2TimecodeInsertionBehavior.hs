{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2TimecodeInsertionBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2TimecodeInsertionBehavior where

import Network.AWS.Prelude

-- | Mpeg2 Timecode Insertion Behavior
data Mpeg2TimecodeInsertionBehavior
  = MTIBDisabled
  | MTIBGopTimecode
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

instance FromText Mpeg2TimecodeInsertionBehavior where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure MTIBDisabled
      "gop_timecode" -> pure MTIBGopTimecode
      e ->
        fromTextError $
          "Failure parsing Mpeg2TimecodeInsertionBehavior from value: '" <> e
            <> "'. Accepted values: disabled, gop_timecode"

instance ToText Mpeg2TimecodeInsertionBehavior where
  toText = \case
    MTIBDisabled -> "DISABLED"
    MTIBGopTimecode -> "GOP_TIMECODE"

instance Hashable Mpeg2TimecodeInsertionBehavior

instance NFData Mpeg2TimecodeInsertionBehavior

instance ToByteString Mpeg2TimecodeInsertionBehavior

instance ToQuery Mpeg2TimecodeInsertionBehavior

instance ToHeader Mpeg2TimecodeInsertionBehavior

instance ToJSON Mpeg2TimecodeInsertionBehavior where
  toJSON = toJSONText

instance FromJSON Mpeg2TimecodeInsertionBehavior where
  parseJSON = parseJSONText "Mpeg2TimecodeInsertionBehavior"
