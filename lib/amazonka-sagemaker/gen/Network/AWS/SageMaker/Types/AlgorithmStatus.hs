{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmStatus where

import Network.AWS.Prelude

data AlgorithmStatus
  = ACompleted
  | ADeleting
  | AFailed
  | AInProgress
  | APending
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

instance FromText AlgorithmStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure ACompleted
      "deleting" -> pure ADeleting
      "failed" -> pure AFailed
      "inprogress" -> pure AInProgress
      "pending" -> pure APending
      e ->
        fromTextError $
          "Failure parsing AlgorithmStatus from value: '" <> e
            <> "'. Accepted values: completed, deleting, failed, inprogress, pending"

instance ToText AlgorithmStatus where
  toText = \case
    ACompleted -> "Completed"
    ADeleting -> "Deleting"
    AFailed -> "Failed"
    AInProgress -> "InProgress"
    APending -> "Pending"

instance Hashable AlgorithmStatus

instance NFData AlgorithmStatus

instance ToByteString AlgorithmStatus

instance ToQuery AlgorithmStatus

instance ToHeader AlgorithmStatus

instance FromJSON AlgorithmStatus where
  parseJSON = parseJSONText "AlgorithmStatus"
