{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TaskRunSortColumnType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskRunSortColumnType where

import Network.AWS.Prelude

data TaskRunSortColumnType
  = TRSCTStarted
  | TRSCTStatus
  | TRSCTTaskRunType
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

instance FromText TaskRunSortColumnType where
  parser =
    takeLowerText >>= \case
      "started" -> pure TRSCTStarted
      "status" -> pure TRSCTStatus
      "task_run_type" -> pure TRSCTTaskRunType
      e ->
        fromTextError $
          "Failure parsing TaskRunSortColumnType from value: '" <> e
            <> "'. Accepted values: started, status, task_run_type"

instance ToText TaskRunSortColumnType where
  toText = \case
    TRSCTStarted -> "STARTED"
    TRSCTStatus -> "STATUS"
    TRSCTTaskRunType -> "TASK_RUN_TYPE"

instance Hashable TaskRunSortColumnType

instance NFData TaskRunSortColumnType

instance ToByteString TaskRunSortColumnType

instance ToQuery TaskRunSortColumnType

instance ToHeader TaskRunSortColumnType

instance ToJSON TaskRunSortColumnType where
  toJSON = toJSONText
