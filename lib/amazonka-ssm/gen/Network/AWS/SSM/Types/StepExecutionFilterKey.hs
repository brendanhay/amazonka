{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.StepExecutionFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.StepExecutionFilterKey
  ( StepExecutionFilterKey
      ( StepExecutionFilterKey',
        StepExecutionFilterKeyStartTimeBefore,
        StepExecutionFilterKeyStartTimeAfter,
        StepExecutionFilterKeyStepExecutionStatus,
        StepExecutionFilterKeyStepExecutionId,
        StepExecutionFilterKeyStepName,
        StepExecutionFilterKeyAction,
        fromStepExecutionFilterKey
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype StepExecutionFilterKey = StepExecutionFilterKey'
  { fromStepExecutionFilterKey ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern StepExecutionFilterKeyStartTimeBefore :: StepExecutionFilterKey
pattern StepExecutionFilterKeyStartTimeBefore = StepExecutionFilterKey' "StartTimeBefore"

pattern StepExecutionFilterKeyStartTimeAfter :: StepExecutionFilterKey
pattern StepExecutionFilterKeyStartTimeAfter = StepExecutionFilterKey' "StartTimeAfter"

pattern StepExecutionFilterKeyStepExecutionStatus :: StepExecutionFilterKey
pattern StepExecutionFilterKeyStepExecutionStatus = StepExecutionFilterKey' "StepExecutionStatus"

pattern StepExecutionFilterKeyStepExecutionId :: StepExecutionFilterKey
pattern StepExecutionFilterKeyStepExecutionId = StepExecutionFilterKey' "StepExecutionId"

pattern StepExecutionFilterKeyStepName :: StepExecutionFilterKey
pattern StepExecutionFilterKeyStepName = StepExecutionFilterKey' "StepName"

pattern StepExecutionFilterKeyAction :: StepExecutionFilterKey
pattern StepExecutionFilterKeyAction = StepExecutionFilterKey' "Action"

{-# COMPLETE
  StepExecutionFilterKeyStartTimeBefore,
  StepExecutionFilterKeyStartTimeAfter,
  StepExecutionFilterKeyStepExecutionStatus,
  StepExecutionFilterKeyStepExecutionId,
  StepExecutionFilterKeyStepName,
  StepExecutionFilterKeyAction,
  StepExecutionFilterKey'
  #-}
