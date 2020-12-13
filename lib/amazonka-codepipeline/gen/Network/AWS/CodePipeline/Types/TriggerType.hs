{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.TriggerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.TriggerType
  ( TriggerType
      ( TriggerType',
        CreatePipeline,
        StartPipelineExecution,
        PollForSourceChanges,
        Webhook,
        CloudWatchEvent,
        PutActionRevision
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TriggerType = TriggerType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CreatePipeline :: TriggerType
pattern CreatePipeline = TriggerType' "CreatePipeline"

pattern StartPipelineExecution :: TriggerType
pattern StartPipelineExecution = TriggerType' "StartPipelineExecution"

pattern PollForSourceChanges :: TriggerType
pattern PollForSourceChanges = TriggerType' "PollForSourceChanges"

pattern Webhook :: TriggerType
pattern Webhook = TriggerType' "Webhook"

pattern CloudWatchEvent :: TriggerType
pattern CloudWatchEvent = TriggerType' "CloudWatchEvent"

pattern PutActionRevision :: TriggerType
pattern PutActionRevision = TriggerType' "PutActionRevision"

{-# COMPLETE
  CreatePipeline,
  StartPipelineExecution,
  PollForSourceChanges,
  Webhook,
  CloudWatchEvent,
  PutActionRevision,
  TriggerType'
  #-}
