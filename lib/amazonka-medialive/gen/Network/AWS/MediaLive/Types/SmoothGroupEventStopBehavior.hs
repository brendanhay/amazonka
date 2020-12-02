{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupEventStopBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupEventStopBehavior where

import Network.AWS.Prelude

-- | Smooth Group Event Stop Behavior
data SmoothGroupEventStopBehavior
  = SGESBNone
  | SGESBSendEos
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

instance FromText SmoothGroupEventStopBehavior where
  parser =
    takeLowerText >>= \case
      "none" -> pure SGESBNone
      "send_eos" -> pure SGESBSendEos
      e ->
        fromTextError $
          "Failure parsing SmoothGroupEventStopBehavior from value: '" <> e
            <> "'. Accepted values: none, send_eos"

instance ToText SmoothGroupEventStopBehavior where
  toText = \case
    SGESBNone -> "NONE"
    SGESBSendEos -> "SEND_EOS"

instance Hashable SmoothGroupEventStopBehavior

instance NFData SmoothGroupEventStopBehavior

instance ToByteString SmoothGroupEventStopBehavior

instance ToQuery SmoothGroupEventStopBehavior

instance ToHeader SmoothGroupEventStopBehavior

instance ToJSON SmoothGroupEventStopBehavior where
  toJSON = toJSONText

instance FromJSON SmoothGroupEventStopBehavior where
  parseJSON = parseJSONText "SmoothGroupEventStopBehavior"
