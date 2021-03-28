{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AutomationExecutionFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AutomationExecutionFilterKey
  ( AutomationExecutionFilterKey
    ( AutomationExecutionFilterKey'
    , AutomationExecutionFilterKeyDocumentNamePrefix
    , AutomationExecutionFilterKeyExecutionStatus
    , AutomationExecutionFilterKeyExecutionId
    , AutomationExecutionFilterKeyParentExecutionId
    , AutomationExecutionFilterKeyCurrentAction
    , AutomationExecutionFilterKeyStartTimeBefore
    , AutomationExecutionFilterKeyStartTimeAfter
    , AutomationExecutionFilterKeyAutomationType
    , AutomationExecutionFilterKeyTagKey
    , AutomationExecutionFilterKeyTargetResourceGroup
    , fromAutomationExecutionFilterKey
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AutomationExecutionFilterKey = AutomationExecutionFilterKey'{fromAutomationExecutionFilterKey
                                                                     :: Core.Text}
                                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                         Core.Generic)
                                         deriving newtype (Core.IsString, Core.Hashable,
                                                           Core.NFData, Core.ToJSONKey,
                                                           Core.FromJSONKey, Core.ToJSON,
                                                           Core.FromJSON, Core.ToXML, Core.FromXML,
                                                           Core.ToText, Core.FromText,
                                                           Core.ToByteString, Core.ToQuery,
                                                           Core.ToHeader)

pattern AutomationExecutionFilterKeyDocumentNamePrefix :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKeyDocumentNamePrefix = AutomationExecutionFilterKey' "DocumentNamePrefix"

pattern AutomationExecutionFilterKeyExecutionStatus :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKeyExecutionStatus = AutomationExecutionFilterKey' "ExecutionStatus"

pattern AutomationExecutionFilterKeyExecutionId :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKeyExecutionId = AutomationExecutionFilterKey' "ExecutionId"

pattern AutomationExecutionFilterKeyParentExecutionId :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKeyParentExecutionId = AutomationExecutionFilterKey' "ParentExecutionId"

pattern AutomationExecutionFilterKeyCurrentAction :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKeyCurrentAction = AutomationExecutionFilterKey' "CurrentAction"

pattern AutomationExecutionFilterKeyStartTimeBefore :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKeyStartTimeBefore = AutomationExecutionFilterKey' "StartTimeBefore"

pattern AutomationExecutionFilterKeyStartTimeAfter :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKeyStartTimeAfter = AutomationExecutionFilterKey' "StartTimeAfter"

pattern AutomationExecutionFilterKeyAutomationType :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKeyAutomationType = AutomationExecutionFilterKey' "AutomationType"

pattern AutomationExecutionFilterKeyTagKey :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKeyTagKey = AutomationExecutionFilterKey' "TagKey"

pattern AutomationExecutionFilterKeyTargetResourceGroup :: AutomationExecutionFilterKey
pattern AutomationExecutionFilterKeyTargetResourceGroup = AutomationExecutionFilterKey' "TargetResourceGroup"

{-# COMPLETE 
  AutomationExecutionFilterKeyDocumentNamePrefix,

  AutomationExecutionFilterKeyExecutionStatus,

  AutomationExecutionFilterKeyExecutionId,

  AutomationExecutionFilterKeyParentExecutionId,

  AutomationExecutionFilterKeyCurrentAction,

  AutomationExecutionFilterKeyStartTimeBefore,

  AutomationExecutionFilterKeyStartTimeAfter,

  AutomationExecutionFilterKeyAutomationType,

  AutomationExecutionFilterKeyTagKey,

  AutomationExecutionFilterKeyTargetResourceGroup,
  AutomationExecutionFilterKey'
  #-}
