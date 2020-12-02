{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsIncompleteSegmentBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsIncompleteSegmentBehavior where

import Network.AWS.Prelude

-- | Hls Incomplete Segment Behavior
data HlsIncompleteSegmentBehavior
  = HISBAuto
  | HISBSuppress
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

instance FromText HlsIncompleteSegmentBehavior where
  parser =
    takeLowerText >>= \case
      "auto" -> pure HISBAuto
      "suppress" -> pure HISBSuppress
      e ->
        fromTextError $
          "Failure parsing HlsIncompleteSegmentBehavior from value: '" <> e
            <> "'. Accepted values: auto, suppress"

instance ToText HlsIncompleteSegmentBehavior where
  toText = \case
    HISBAuto -> "AUTO"
    HISBSuppress -> "SUPPRESS"

instance Hashable HlsIncompleteSegmentBehavior

instance NFData HlsIncompleteSegmentBehavior

instance ToByteString HlsIncompleteSegmentBehavior

instance ToQuery HlsIncompleteSegmentBehavior

instance ToHeader HlsIncompleteSegmentBehavior

instance ToJSON HlsIncompleteSegmentBehavior where
  toJSON = toJSONText

instance FromJSON HlsIncompleteSegmentBehavior where
  parseJSON = parseJSONText "HlsIncompleteSegmentBehavior"
