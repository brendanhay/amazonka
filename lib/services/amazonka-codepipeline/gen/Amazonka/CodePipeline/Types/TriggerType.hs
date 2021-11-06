{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodePipeline.Types.TriggerType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.TriggerType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TriggerType = TriggerType'
  { fromTriggerType ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
