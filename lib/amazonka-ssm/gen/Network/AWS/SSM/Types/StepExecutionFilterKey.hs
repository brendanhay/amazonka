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
        StartTimeBefore,
        StartTimeAfter,
        StepExecutionStatus,
        StepExecutionId,
        StepName,
        Action
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StepExecutionFilterKey = StepExecutionFilterKey' Lude.Text
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

pattern StartTimeBefore :: StepExecutionFilterKey
pattern StartTimeBefore = StepExecutionFilterKey' "StartTimeBefore"

pattern StartTimeAfter :: StepExecutionFilterKey
pattern StartTimeAfter = StepExecutionFilterKey' "StartTimeAfter"

pattern StepExecutionStatus :: StepExecutionFilterKey
pattern StepExecutionStatus = StepExecutionFilterKey' "StepExecutionStatus"

pattern StepExecutionId :: StepExecutionFilterKey
pattern StepExecutionId = StepExecutionFilterKey' "StepExecutionId"

pattern StepName :: StepExecutionFilterKey
pattern StepName = StepExecutionFilterKey' "StepName"

pattern Action :: StepExecutionFilterKey
pattern Action = StepExecutionFilterKey' "Action"

{-# COMPLETE
  StartTimeBefore,
  StartTimeAfter,
  StepExecutionStatus,
  StepExecutionId,
  StepName,
  Action,
  StepExecutionFilterKey'
  #-}
