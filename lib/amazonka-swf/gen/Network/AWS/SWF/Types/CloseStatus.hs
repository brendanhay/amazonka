{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.CloseStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.CloseStatus where

import Network.AWS.Prelude

data CloseStatus
  = Canceled
  | Completed
  | ContinuedAsNew
  | Failed
  | Terminated
  | TimedOut
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

instance FromText CloseStatus where
  parser =
    takeLowerText >>= \case
      "canceled" -> pure Canceled
      "completed" -> pure Completed
      "continued_as_new" -> pure ContinuedAsNew
      "failed" -> pure Failed
      "terminated" -> pure Terminated
      "timed_out" -> pure TimedOut
      e ->
        fromTextError $
          "Failure parsing CloseStatus from value: '" <> e
            <> "'. Accepted values: canceled, completed, continued_as_new, failed, terminated, timed_out"

instance ToText CloseStatus where
  toText = \case
    Canceled -> "CANCELED"
    Completed -> "COMPLETED"
    ContinuedAsNew -> "CONTINUED_AS_NEW"
    Failed -> "FAILED"
    Terminated -> "TERMINATED"
    TimedOut -> "TIMED_OUT"

instance Hashable CloseStatus

instance NFData CloseStatus

instance ToByteString CloseStatus

instance ToQuery CloseStatus

instance ToHeader CloseStatus

instance ToJSON CloseStatus where
  toJSON = toJSONText

instance FromJSON CloseStatus where
  parseJSON = parseJSONText "CloseStatus"
