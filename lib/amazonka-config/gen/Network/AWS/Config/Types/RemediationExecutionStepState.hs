{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExecutionStepState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExecutionStepState
  ( RemediationExecutionStepState
      ( RemediationExecutionStepState',
        RemediationExecutionStepStateSucceeded,
        RemediationExecutionStepStatePending,
        RemediationExecutionStepStateFailed,
        fromRemediationExecutionStepState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype RemediationExecutionStepState = RemediationExecutionStepState'
  { fromRemediationExecutionStepState ::
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

pattern RemediationExecutionStepStateSucceeded :: RemediationExecutionStepState
pattern RemediationExecutionStepStateSucceeded = RemediationExecutionStepState' "SUCCEEDED"

pattern RemediationExecutionStepStatePending :: RemediationExecutionStepState
pattern RemediationExecutionStepStatePending = RemediationExecutionStepState' "PENDING"

pattern RemediationExecutionStepStateFailed :: RemediationExecutionStepState
pattern RemediationExecutionStepStateFailed = RemediationExecutionStepState' "FAILED"

{-# COMPLETE
  RemediationExecutionStepStateSucceeded,
  RemediationExecutionStepStatePending,
  RemediationExecutionStepStateFailed,
  RemediationExecutionStepState'
  #-}
