{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.LastFrameClippingBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.LastFrameClippingBehavior where

import Network.AWS.Prelude

-- | If you specify a StopTimecode in an input (in order to clip the file), you can specify if you want the clip to exclude (the default) or include the frame specified by the timecode.
data LastFrameClippingBehavior
  = ExcludeLastFrame
  | IncludeLastFrame
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

instance FromText LastFrameClippingBehavior where
  parser =
    takeLowerText >>= \case
      "exclude_last_frame" -> pure ExcludeLastFrame
      "include_last_frame" -> pure IncludeLastFrame
      e ->
        fromTextError $
          "Failure parsing LastFrameClippingBehavior from value: '" <> e
            <> "'. Accepted values: exclude_last_frame, include_last_frame"

instance ToText LastFrameClippingBehavior where
  toText = \case
    ExcludeLastFrame -> "EXCLUDE_LAST_FRAME"
    IncludeLastFrame -> "INCLUDE_LAST_FRAME"

instance Hashable LastFrameClippingBehavior

instance NFData LastFrameClippingBehavior

instance ToByteString LastFrameClippingBehavior

instance ToQuery LastFrameClippingBehavior

instance ToHeader LastFrameClippingBehavior

instance ToJSON LastFrameClippingBehavior where
  toJSON = toJSONText

instance FromJSON LastFrameClippingBehavior where
  parseJSON = parseJSONText "LastFrameClippingBehavior"
