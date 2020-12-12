{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ExecutionStatus
  ( ExecutionStatus
      ( ExecutionStatus',
        ECompleted,
        ECompletedWithViolations,
        EFailed,
        EInProgress,
        EPending,
        EStopped,
        EStopping
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ExecutionStatus = ExecutionStatus' Lude.Text
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

pattern ECompleted :: ExecutionStatus
pattern ECompleted = ExecutionStatus' "Completed"

pattern ECompletedWithViolations :: ExecutionStatus
pattern ECompletedWithViolations = ExecutionStatus' "CompletedWithViolations"

pattern EFailed :: ExecutionStatus
pattern EFailed = ExecutionStatus' "Failed"

pattern EInProgress :: ExecutionStatus
pattern EInProgress = ExecutionStatus' "InProgress"

pattern EPending :: ExecutionStatus
pattern EPending = ExecutionStatus' "Pending"

pattern EStopped :: ExecutionStatus
pattern EStopped = ExecutionStatus' "Stopped"

pattern EStopping :: ExecutionStatus
pattern EStopping = ExecutionStatus' "Stopping"

{-# COMPLETE
  ECompleted,
  ECompletedWithViolations,
  EFailed,
  EInProgress,
  EPending,
  EStopped,
  EStopping,
  ExecutionStatus'
  #-}
