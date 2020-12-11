-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.InstanceRefreshStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstanceRefreshStatus
  ( InstanceRefreshStatus
      ( InstanceRefreshStatus',
        IRSCancelled,
        IRSCancelling,
        IRSFailed,
        IRSInProgress,
        IRSPending,
        IRSSuccessful
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstanceRefreshStatus = InstanceRefreshStatus' Lude.Text
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

pattern IRSCancelled :: InstanceRefreshStatus
pattern IRSCancelled = InstanceRefreshStatus' "Cancelled"

pattern IRSCancelling :: InstanceRefreshStatus
pattern IRSCancelling = InstanceRefreshStatus' "Cancelling"

pattern IRSFailed :: InstanceRefreshStatus
pattern IRSFailed = InstanceRefreshStatus' "Failed"

pattern IRSInProgress :: InstanceRefreshStatus
pattern IRSInProgress = InstanceRefreshStatus' "InProgress"

pattern IRSPending :: InstanceRefreshStatus
pattern IRSPending = InstanceRefreshStatus' "Pending"

pattern IRSSuccessful :: InstanceRefreshStatus
pattern IRSSuccessful = InstanceRefreshStatus' "Successful"

{-# COMPLETE
  IRSCancelled,
  IRSCancelling,
  IRSFailed,
  IRSInProgress,
  IRSPending,
  IRSSuccessful,
  InstanceRefreshStatus'
  #-}
