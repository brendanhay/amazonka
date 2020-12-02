{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ChannelStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ChannelStatus where

import Network.AWS.Prelude

data ChannelStatus
  = Created
  | Failed
  | InProgress
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

instance FromText ChannelStatus where
  parser =
    takeLowerText >>= \case
      "created" -> pure Created
      "failed" -> pure Failed
      "in_progress" -> pure InProgress
      e ->
        fromTextError $
          "Failure parsing ChannelStatus from value: '" <> e
            <> "'. Accepted values: created, failed, in_progress"

instance ToText ChannelStatus where
  toText = \case
    Created -> "CREATED"
    Failed -> "FAILED"
    InProgress -> "IN_PROGRESS"

instance Hashable ChannelStatus

instance NFData ChannelStatus

instance ToByteString ChannelStatus

instance ToQuery ChannelStatus

instance ToHeader ChannelStatus

instance FromJSON ChannelStatus where
  parseJSON = parseJSONText "ChannelStatus"
