{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.QueueStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.QueueStatus where

import Network.AWS.Prelude

-- | Queues can be ACTIVE or PAUSED. If you pause a queue, jobs in that queue won't begin. Jobs that are running when you pause a queue continue to run until they finish or result in an error.
data QueueStatus
  = QSActive
  | QSPaused
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

instance FromText QueueStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure QSActive
      "paused" -> pure QSPaused
      e ->
        fromTextError $
          "Failure parsing QueueStatus from value: '" <> e
            <> "'. Accepted values: active, paused"

instance ToText QueueStatus where
  toText = \case
    QSActive -> "ACTIVE"
    QSPaused -> "PAUSED"

instance Hashable QueueStatus

instance NFData QueueStatus

instance ToByteString QueueStatus

instance ToQuery QueueStatus

instance ToHeader QueueStatus

instance ToJSON QueueStatus where
  toJSON = toJSONText

instance FromJSON QueueStatus where
  parseJSON = parseJSONText "QueueStatus"
