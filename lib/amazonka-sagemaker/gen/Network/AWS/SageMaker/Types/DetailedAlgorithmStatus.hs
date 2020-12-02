{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DetailedAlgorithmStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DetailedAlgorithmStatus where

import Network.AWS.Prelude

data DetailedAlgorithmStatus
  = DASCompleted
  | DASFailed
  | DASInProgress
  | DASNotStarted
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

instance FromText DetailedAlgorithmStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure DASCompleted
      "failed" -> pure DASFailed
      "inprogress" -> pure DASInProgress
      "notstarted" -> pure DASNotStarted
      e ->
        fromTextError $
          "Failure parsing DetailedAlgorithmStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, inprogress, notstarted"

instance ToText DetailedAlgorithmStatus where
  toText = \case
    DASCompleted -> "Completed"
    DASFailed -> "Failed"
    DASInProgress -> "InProgress"
    DASNotStarted -> "NotStarted"

instance Hashable DetailedAlgorithmStatus

instance NFData DetailedAlgorithmStatus

instance ToByteString DetailedAlgorithmStatus

instance ToQuery DetailedAlgorithmStatus

instance ToHeader DetailedAlgorithmStatus

instance FromJSON DetailedAlgorithmStatus where
  parseJSON = parseJSONText "DetailedAlgorithmStatus"
