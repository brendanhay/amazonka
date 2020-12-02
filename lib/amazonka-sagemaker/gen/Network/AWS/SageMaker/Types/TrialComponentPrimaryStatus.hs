{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentPrimaryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentPrimaryStatus where

import Network.AWS.Prelude

data TrialComponentPrimaryStatus
  = TCPSCompleted
  | TCPSFailed
  | TCPSInProgress
  | TCPSStopped
  | TCPSStopping
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

instance FromText TrialComponentPrimaryStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure TCPSCompleted
      "failed" -> pure TCPSFailed
      "inprogress" -> pure TCPSInProgress
      "stopped" -> pure TCPSStopped
      "stopping" -> pure TCPSStopping
      e ->
        fromTextError $
          "Failure parsing TrialComponentPrimaryStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, inprogress, stopped, stopping"

instance ToText TrialComponentPrimaryStatus where
  toText = \case
    TCPSCompleted -> "Completed"
    TCPSFailed -> "Failed"
    TCPSInProgress -> "InProgress"
    TCPSStopped -> "Stopped"
    TCPSStopping -> "Stopping"

instance Hashable TrialComponentPrimaryStatus

instance NFData TrialComponentPrimaryStatus

instance ToByteString TrialComponentPrimaryStatus

instance ToQuery TrialComponentPrimaryStatus

instance ToHeader TrialComponentPrimaryStatus

instance ToJSON TrialComponentPrimaryStatus where
  toJSON = toJSONText

instance FromJSON TrialComponentPrimaryStatus where
  parseJSON = parseJSONText "TrialComponentPrimaryStatus"
