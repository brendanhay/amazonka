{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackInstanceDetailedStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackInstanceDetailedStatus
  ( StackInstanceDetailedStatus
      ( StackInstanceDetailedStatus',
        StackInstanceDetailedStatusPending,
        StackInstanceDetailedStatusRunning,
        StackInstanceDetailedStatusSucceeded,
        StackInstanceDetailedStatusFailed,
        StackInstanceDetailedStatusCancelled,
        StackInstanceDetailedStatusInoperable,
        fromStackInstanceDetailedStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype StackInstanceDetailedStatus = StackInstanceDetailedStatus'
  { fromStackInstanceDetailedStatus ::
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

pattern StackInstanceDetailedStatusPending :: StackInstanceDetailedStatus
pattern StackInstanceDetailedStatusPending = StackInstanceDetailedStatus' "PENDING"

pattern StackInstanceDetailedStatusRunning :: StackInstanceDetailedStatus
pattern StackInstanceDetailedStatusRunning = StackInstanceDetailedStatus' "RUNNING"

pattern StackInstanceDetailedStatusSucceeded :: StackInstanceDetailedStatus
pattern StackInstanceDetailedStatusSucceeded = StackInstanceDetailedStatus' "SUCCEEDED"

pattern StackInstanceDetailedStatusFailed :: StackInstanceDetailedStatus
pattern StackInstanceDetailedStatusFailed = StackInstanceDetailedStatus' "FAILED"

pattern StackInstanceDetailedStatusCancelled :: StackInstanceDetailedStatus
pattern StackInstanceDetailedStatusCancelled = StackInstanceDetailedStatus' "CANCELLED"

pattern StackInstanceDetailedStatusInoperable :: StackInstanceDetailedStatus
pattern StackInstanceDetailedStatusInoperable = StackInstanceDetailedStatus' "INOPERABLE"

{-# COMPLETE
  StackInstanceDetailedStatusPending,
  StackInstanceDetailedStatusRunning,
  StackInstanceDetailedStatusSucceeded,
  StackInstanceDetailedStatusFailed,
  StackInstanceDetailedStatusCancelled,
  StackInstanceDetailedStatusInoperable,
  StackInstanceDetailedStatus'
  #-}
