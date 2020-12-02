{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.SmoothGroupStreamManifestBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.SmoothGroupStreamManifestBehavior where

import Network.AWS.Prelude

-- | Smooth Group Stream Manifest Behavior
data SmoothGroupStreamManifestBehavior
  = DoNotSend
  | Send
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

instance FromText SmoothGroupStreamManifestBehavior where
  parser =
    takeLowerText >>= \case
      "do_not_send" -> pure DoNotSend
      "send" -> pure Send
      e ->
        fromTextError $
          "Failure parsing SmoothGroupStreamManifestBehavior from value: '" <> e
            <> "'. Accepted values: do_not_send, send"

instance ToText SmoothGroupStreamManifestBehavior where
  toText = \case
    DoNotSend -> "DO_NOT_SEND"
    Send -> "SEND"

instance Hashable SmoothGroupStreamManifestBehavior

instance NFData SmoothGroupStreamManifestBehavior

instance ToByteString SmoothGroupStreamManifestBehavior

instance ToQuery SmoothGroupStreamManifestBehavior

instance ToHeader SmoothGroupStreamManifestBehavior

instance ToJSON SmoothGroupStreamManifestBehavior where
  toJSON = toJSONText

instance FromJSON SmoothGroupStreamManifestBehavior where
  parseJSON = parseJSONText "SmoothGroupStreamManifestBehavior"
