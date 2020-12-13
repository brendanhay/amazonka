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
        SIDSPending,
        SIDSRunning,
        SIDSSucceeded,
        SIDSFailed,
        SIDSCancelled,
        SIDSInoperable
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StackInstanceDetailedStatus = StackInstanceDetailedStatus' Lude.Text
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

pattern SIDSPending :: StackInstanceDetailedStatus
pattern SIDSPending = StackInstanceDetailedStatus' "PENDING"

pattern SIDSRunning :: StackInstanceDetailedStatus
pattern SIDSRunning = StackInstanceDetailedStatus' "RUNNING"

pattern SIDSSucceeded :: StackInstanceDetailedStatus
pattern SIDSSucceeded = StackInstanceDetailedStatus' "SUCCEEDED"

pattern SIDSFailed :: StackInstanceDetailedStatus
pattern SIDSFailed = StackInstanceDetailedStatus' "FAILED"

pattern SIDSCancelled :: StackInstanceDetailedStatus
pattern SIDSCancelled = StackInstanceDetailedStatus' "CANCELLED"

pattern SIDSInoperable :: StackInstanceDetailedStatus
pattern SIDSInoperable = StackInstanceDetailedStatus' "INOPERABLE"

{-# COMPLETE
  SIDSPending,
  SIDSRunning,
  SIDSSucceeded,
  SIDSFailed,
  SIDSCancelled,
  SIDSInoperable,
  StackInstanceDetailedStatus'
  #-}
