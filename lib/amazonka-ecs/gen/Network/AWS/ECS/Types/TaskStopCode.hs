{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskStopCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskStopCode where

import Network.AWS.Prelude

data TaskStopCode
  = EssentialContainerExited
  | TaskFailedToStart
  | UserInitiated
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

instance FromText TaskStopCode where
  parser =
    takeLowerText >>= \case
      "essentialcontainerexited" -> pure EssentialContainerExited
      "taskfailedtostart" -> pure TaskFailedToStart
      "userinitiated" -> pure UserInitiated
      e ->
        fromTextError $
          "Failure parsing TaskStopCode from value: '" <> e
            <> "'. Accepted values: essentialcontainerexited, taskfailedtostart, userinitiated"

instance ToText TaskStopCode where
  toText = \case
    EssentialContainerExited -> "EssentialContainerExited"
    TaskFailedToStart -> "TaskFailedToStart"
    UserInitiated -> "UserInitiated"

instance Hashable TaskStopCode

instance NFData TaskStopCode

instance ToByteString TaskStopCode

instance ToQuery TaskStopCode

instance ToHeader TaskStopCode

instance FromJSON TaskStopCode where
  parseJSON = parseJSONText "TaskStopCode"
