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
-- Module      : Network.AWS.SSM.Types.AutomationExecutionFilterKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AutomationExecutionFilterKey
  ( AutomationExecutionFilterKey
      ( ..,
        AutomationExecutionFilterKey_AutomationSubtype,
        AutomationExecutionFilterKey_AutomationType,
        AutomationExecutionFilterKey_CurrentAction,
        AutomationExecutionFilterKey_DocumentNamePrefix,
        AutomationExecutionFilterKey_ExecutionId,
        AutomationExecutionFilterKey_ExecutionStatus,
        AutomationExecutionFilterKey_OpsItemId,
        AutomationExecutionFilterKey_ParentExecutionId,
        AutomationExecutionFilterKey_StartTimeAfter,
        AutomationExecutionFilterKey_StartTimeBefore,
        AutomationExecutionFilterKey_TagKey,
        AutomationExecutionFilterKey_TargetResourceGroup
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AutomationExecutionFilterKey = AutomationExecutionFilterKey'
  { fromAutomationExecutionFilterKey ::
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

pattern AutomationExecutionFilterKey_AutomationSubtype :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKey_AutomationSubtype = AutomationExecutionFilterKey' "AutomationSubtype"

pattern AutomationExecutionFilterKey_AutomationType :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKey_AutomationType = AutomationExecutionFilterKey' "AutomationType"

pattern AutomationExecutionFilterKey_CurrentAction :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKey_CurrentAction = AutomationExecutionFilterKey' "CurrentAction"

pattern AutomationExecutionFilterKey_DocumentNamePrefix :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKey_DocumentNamePrefix = AutomationExecutionFilterKey' "DocumentNamePrefix"

pattern AutomationExecutionFilterKey_ExecutionId :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKey_ExecutionId = AutomationExecutionFilterKey' "ExecutionId"

pattern AutomationExecutionFilterKey_ExecutionStatus :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKey_ExecutionStatus = AutomationExecutionFilterKey' "ExecutionStatus"

pattern AutomationExecutionFilterKey_OpsItemId :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKey_OpsItemId = AutomationExecutionFilterKey' "OpsItemId"

pattern AutomationExecutionFilterKey_ParentExecutionId :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKey_ParentExecutionId = AutomationExecutionFilterKey' "ParentExecutionId"

pattern AutomationExecutionFilterKey_StartTimeAfter :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKey_StartTimeAfter = AutomationExecutionFilterKey' "StartTimeAfter"

pattern AutomationExecutionFilterKey_StartTimeBefore :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKey_StartTimeBefore = AutomationExecutionFilterKey' "StartTimeBefore"

pattern AutomationExecutionFilterKey_TagKey :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKey_TagKey = AutomationExecutionFilterKey' "TagKey"

pattern AutomationExecutionFilterKey_TargetResourceGroup :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKey_TargetResourceGroup = AutomationExecutionFilterKey' "TargetResourceGroup"

{-# COMPLETE
  AutomationExecutionFilterKey_AutomationSubtype,
  AutomationExecutionFilterKey_AutomationType,
  AutomationExecutionFilterKey_CurrentAction,
  AutomationExecutionFilterKey_DocumentNamePrefix,
  AutomationExecutionFilterKey_ExecutionId,
  AutomationExecutionFilterKey_ExecutionStatus,
  AutomationExecutionFilterKey_OpsItemId,
  AutomationExecutionFilterKey_ParentExecutionId,
  AutomationExecutionFilterKey_StartTimeAfter,
  AutomationExecutionFilterKey_StartTimeBefore,
  AutomationExecutionFilterKey_TagKey,
  AutomationExecutionFilterKey_TargetResourceGroup,
  AutomationExecutionFilterKey'
  #-}
