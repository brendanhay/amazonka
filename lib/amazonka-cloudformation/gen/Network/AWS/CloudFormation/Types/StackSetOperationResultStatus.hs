{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        Pending,
        Running,
        Succeeded,
        Failed,
        Cancelled
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

pattern Pending :: StackSetOperationResultStatus
pattern Pending = StackSetOperationResultStatus' "PENDING"

pattern Running :: StackSetOperationResultStatus
pattern Running = StackSetOperationResultStatus' "RUNNING"

pattern Succeeded :: StackSetOperationResultStatus
pattern Succeeded = StackSetOperationResultStatus' "SUCCEEDED"

pattern Failed :: StackSetOperationResultStatus
pattern Failed = StackSetOperationResultStatus' "FAILED"

pattern Cancelled :: StackSetOperationResultStatus
pattern Cancelled = StackSetOperationResultStatus' "CANCELLED"

{-# COMPLETE
  Pending,
  Running,
  Succeeded,
  Failed,
  Cancelled,
  StackSetOperationResultStatus'
  #-}
