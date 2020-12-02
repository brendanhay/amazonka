{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FollowPoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FollowPoint where

import Network.AWS.Prelude

-- | Follow reference point.
data FollowPoint
  = End
  | Start
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

instance FromText FollowPoint where
  parser =
    takeLowerText >>= \case
      "end" -> pure End
      "start" -> pure Start
      e ->
        fromTextError $
          "Failure parsing FollowPoint from value: '" <> e
            <> "'. Accepted values: end, start"

instance ToText FollowPoint where
  toText = \case
    End -> "END"
    Start -> "START"

instance Hashable FollowPoint

instance NFData FollowPoint

instance ToByteString FollowPoint

instance ToQuery FollowPoint

instance ToHeader FollowPoint

instance ToJSON FollowPoint where
  toJSON = toJSONText

instance FromJSON FollowPoint where
  parseJSON = parseJSONText "FollowPoint"
