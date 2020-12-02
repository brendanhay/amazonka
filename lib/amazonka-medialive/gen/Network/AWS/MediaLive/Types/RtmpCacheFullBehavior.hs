{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.RtmpCacheFullBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RtmpCacheFullBehavior where

import Network.AWS.Prelude

-- | Rtmp Cache Full Behavior
data RtmpCacheFullBehavior
  = DisconnectImmediately
  | WaitForServer
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

instance FromText RtmpCacheFullBehavior where
  parser =
    takeLowerText >>= \case
      "disconnect_immediately" -> pure DisconnectImmediately
      "wait_for_server" -> pure WaitForServer
      e ->
        fromTextError $
          "Failure parsing RtmpCacheFullBehavior from value: '" <> e
            <> "'. Accepted values: disconnect_immediately, wait_for_server"

instance ToText RtmpCacheFullBehavior where
  toText = \case
    DisconnectImmediately -> "DISCONNECT_IMMEDIATELY"
    WaitForServer -> "WAIT_FOR_SERVER"

instance Hashable RtmpCacheFullBehavior

instance NFData RtmpCacheFullBehavior

instance ToByteString RtmpCacheFullBehavior

instance ToQuery RtmpCacheFullBehavior

instance ToHeader RtmpCacheFullBehavior

instance ToJSON RtmpCacheFullBehavior where
  toJSON = toJSONText

instance FromJSON RtmpCacheFullBehavior where
  parseJSON = parseJSONText "RtmpCacheFullBehavior"
