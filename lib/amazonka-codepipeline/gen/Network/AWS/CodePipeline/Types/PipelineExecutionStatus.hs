{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.PipelineExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.PipelineExecutionStatus where

import Network.AWS.Prelude

data PipelineExecutionStatus
  = Failed
  | InProgress
  | Stopped
  | Stopping
  | Succeeded
  | Superseded
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

instance FromText PipelineExecutionStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure Failed
      "inprogress" -> pure InProgress
      "stopped" -> pure Stopped
      "stopping" -> pure Stopping
      "succeeded" -> pure Succeeded
      "superseded" -> pure Superseded
      e ->
        fromTextError $
          "Failure parsing PipelineExecutionStatus from value: '" <> e
            <> "'. Accepted values: failed, inprogress, stopped, stopping, succeeded, superseded"

instance ToText PipelineExecutionStatus where
  toText = \case
    Failed -> "Failed"
    InProgress -> "InProgress"
    Stopped -> "Stopped"
    Stopping -> "Stopping"
    Succeeded -> "Succeeded"
    Superseded -> "Superseded"

instance Hashable PipelineExecutionStatus

instance NFData PipelineExecutionStatus

instance ToByteString PipelineExecutionStatus

instance ToQuery PipelineExecutionStatus

instance ToHeader PipelineExecutionStatus

instance FromJSON PipelineExecutionStatus where
  parseJSON = parseJSONText "PipelineExecutionStatus"
