{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.StatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.StatusType where

import Network.AWS.Prelude

data StatusType
  = Failed
  | Fault
  | InProgress
  | Stopped
  | Succeeded
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

instance FromText StatusType where
  parser =
    takeLowerText >>= \case
      "failed" -> pure Failed
      "fault" -> pure Fault
      "in_progress" -> pure InProgress
      "stopped" -> pure Stopped
      "succeeded" -> pure Succeeded
      "timed_out" -> pure TimedOut
      e ->
        fromTextError $
          "Failure parsing StatusType from value: '" <> e
            <> "'. Accepted values: failed, fault, in_progress, stopped, succeeded, timed_out"

instance ToText StatusType where
  toText = \case
    Failed -> "FAILED"
    Fault -> "FAULT"
    InProgress -> "IN_PROGRESS"
    Stopped -> "STOPPED"
    Succeeded -> "SUCCEEDED"
    TimedOut -> "TIMED_OUT"

instance Hashable StatusType

instance NFData StatusType

instance ToByteString StatusType

instance ToQuery StatusType

instance ToHeader StatusType

instance ToJSON StatusType where
  toJSON = toJSONText

instance FromJSON StatusType where
  parseJSON = parseJSONText "StatusType"
