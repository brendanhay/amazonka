{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CandidateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CandidateStatus where

import Network.AWS.Prelude

data CandidateStatus
  = CSCompleted
  | CSFailed
  | CSInProgress
  | CSStopped
  | CSStopping
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

instance FromText CandidateStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure CSCompleted
      "failed" -> pure CSFailed
      "inprogress" -> pure CSInProgress
      "stopped" -> pure CSStopped
      "stopping" -> pure CSStopping
      e ->
        fromTextError $
          "Failure parsing CandidateStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, inprogress, stopped, stopping"

instance ToText CandidateStatus where
  toText = \case
    CSCompleted -> "Completed"
    CSFailed -> "Failed"
    CSInProgress -> "InProgress"
    CSStopped -> "Stopped"
    CSStopping -> "Stopping"

instance Hashable CandidateStatus

instance NFData CandidateStatus

instance ToByteString CandidateStatus

instance ToQuery CandidateStatus

instance ToHeader CandidateStatus

instance ToJSON CandidateStatus where
  toJSON = toJSONText

instance FromJSON CandidateStatus where
  parseJSON = parseJSONText "CandidateStatus"
