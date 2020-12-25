{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ActionOnFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ActionOnFailure
  ( ActionOnFailure
      ( ActionOnFailure',
        ActionOnFailureTerminateJobFlow,
        ActionOnFailureTerminateCluster,
        ActionOnFailureCancelAndWait,
        ActionOnFailureContinue,
        fromActionOnFailure
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ActionOnFailure = ActionOnFailure'
  { fromActionOnFailure ::
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

pattern ActionOnFailureTerminateJobFlow :: ActionOnFailure
pattern ActionOnFailureTerminateJobFlow = ActionOnFailure' "TERMINATE_JOB_FLOW"

pattern ActionOnFailureTerminateCluster :: ActionOnFailure
pattern ActionOnFailureTerminateCluster = ActionOnFailure' "TERMINATE_CLUSTER"

pattern ActionOnFailureCancelAndWait :: ActionOnFailure
pattern ActionOnFailureCancelAndWait = ActionOnFailure' "CANCEL_AND_WAIT"

pattern ActionOnFailureContinue :: ActionOnFailure
pattern ActionOnFailureContinue = ActionOnFailure' "CONTINUE"

{-# COMPLETE
  ActionOnFailureTerminateJobFlow,
  ActionOnFailureTerminateCluster,
  ActionOnFailureCancelAndWait,
  ActionOnFailureContinue,
  ActionOnFailure'
  #-}
