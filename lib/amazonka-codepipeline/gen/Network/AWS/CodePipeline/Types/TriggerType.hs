{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.TriggerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.TriggerType where

import Network.AWS.Prelude

data TriggerType
  = CloudWatchEvent
  | CreatePipeline
  | PollForSourceChanges
  | PutActionRevision
  | StartPipelineExecution
  | Webhook
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

instance FromText TriggerType where
  parser =
    takeLowerText >>= \case
      "cloudwatchevent" -> pure CloudWatchEvent
      "createpipeline" -> pure CreatePipeline
      "pollforsourcechanges" -> pure PollForSourceChanges
      "putactionrevision" -> pure PutActionRevision
      "startpipelineexecution" -> pure StartPipelineExecution
      "webhook" -> pure Webhook
      e ->
        fromTextError $
          "Failure parsing TriggerType from value: '" <> e
            <> "'. Accepted values: cloudwatchevent, createpipeline, pollforsourcechanges, putactionrevision, startpipelineexecution, webhook"

instance ToText TriggerType where
  toText = \case
    CloudWatchEvent -> "CloudWatchEvent"
    CreatePipeline -> "CreatePipeline"
    PollForSourceChanges -> "PollForSourceChanges"
    PutActionRevision -> "PutActionRevision"
    StartPipelineExecution -> "StartPipelineExecution"
    Webhook -> "Webhook"

instance Hashable TriggerType

instance NFData TriggerType

instance ToByteString TriggerType

instance ToQuery TriggerType

instance ToHeader TriggerType

instance FromJSON TriggerType where
  parseJSON = parseJSONText "TriggerType"
