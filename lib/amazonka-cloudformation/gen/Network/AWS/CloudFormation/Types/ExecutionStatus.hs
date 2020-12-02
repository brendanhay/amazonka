{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ExecutionStatus where

import Network.AWS.Prelude

data ExecutionStatus
  = Available
  | ExecuteComplete
  | ExecuteFailed
  | ExecuteInProgress
  | Obsolete
  | Unavailable
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

instance FromText ExecutionStatus where
  parser =
    takeLowerText >>= \case
      "available" -> pure Available
      "execute_complete" -> pure ExecuteComplete
      "execute_failed" -> pure ExecuteFailed
      "execute_in_progress" -> pure ExecuteInProgress
      "obsolete" -> pure Obsolete
      "unavailable" -> pure Unavailable
      e ->
        fromTextError $
          "Failure parsing ExecutionStatus from value: '" <> e
            <> "'. Accepted values: available, execute_complete, execute_failed, execute_in_progress, obsolete, unavailable"

instance ToText ExecutionStatus where
  toText = \case
    Available -> "AVAILABLE"
    ExecuteComplete -> "EXECUTE_COMPLETE"
    ExecuteFailed -> "EXECUTE_FAILED"
    ExecuteInProgress -> "EXECUTE_IN_PROGRESS"
    Obsolete -> "OBSOLETE"
    Unavailable -> "UNAVAILABLE"

instance Hashable ExecutionStatus

instance NFData ExecutionStatus

instance ToByteString ExecutionStatus

instance ToQuery ExecutionStatus

instance ToHeader ExecutionStatus

instance FromXML ExecutionStatus where
  parseXML = parseXMLText "ExecutionStatus"
