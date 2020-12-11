-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationResultStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperationResultStatus
  ( StackSetOperationResultStatus
      ( StackSetOperationResultStatus',
        Cancelled,
        Failed,
        Pending,
        Running,
        Succeeded
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StackSetOperationResultStatus = StackSetOperationResultStatus' Lude.Text
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

pattern Cancelled :: StackSetOperationResultStatus
pattern Cancelled = StackSetOperationResultStatus' "CANCELLED"

pattern Failed :: StackSetOperationResultStatus
pattern Failed = StackSetOperationResultStatus' "FAILED"

pattern Pending :: StackSetOperationResultStatus
pattern Pending = StackSetOperationResultStatus' "PENDING"

pattern Running :: StackSetOperationResultStatus
pattern Running = StackSetOperationResultStatus' "RUNNING"

pattern Succeeded :: StackSetOperationResultStatus
pattern Succeeded = StackSetOperationResultStatus' "SUCCEEDED"

{-# COMPLETE
  Cancelled,
  Failed,
  Pending,
  Running,
  Succeeded,
  StackSetOperationResultStatus'
  #-}
