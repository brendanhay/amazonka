{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.TriggerType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.TriggerType
  ( TriggerType
      ( ..,
        TriggerType_CloudWatchEvent,
        TriggerType_CreatePipeline,
        TriggerType_PollForSourceChanges,
        TriggerType_PutActionRevision,
        TriggerType_StartPipelineExecution,
        TriggerType_Webhook
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TriggerType = TriggerType'
  { fromTriggerType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern TriggerType_CloudWatchEvent :: TriggerType
pattern TriggerType_CloudWatchEvent = TriggerType' "CloudWatchEvent"

pattern TriggerType_CreatePipeline :: TriggerType
pattern TriggerType_CreatePipeline = TriggerType' "CreatePipeline"

pattern TriggerType_PollForSourceChanges :: TriggerType
pattern TriggerType_PollForSourceChanges = TriggerType' "PollForSourceChanges"

pattern TriggerType_PutActionRevision :: TriggerType
pattern TriggerType_PutActionRevision = TriggerType' "PutActionRevision"

pattern TriggerType_StartPipelineExecution :: TriggerType
pattern TriggerType_StartPipelineExecution = TriggerType' "StartPipelineExecution"

pattern TriggerType_Webhook :: TriggerType
pattern TriggerType_Webhook = TriggerType' "Webhook"

{-# COMPLETE
  TriggerType_CloudWatchEvent,
  TriggerType_CreatePipeline,
  TriggerType_PollForSourceChanges,
  TriggerType_PutActionRevision,
  TriggerType_StartPipelineExecution,
  TriggerType_Webhook,
  TriggerType'
  #-}
