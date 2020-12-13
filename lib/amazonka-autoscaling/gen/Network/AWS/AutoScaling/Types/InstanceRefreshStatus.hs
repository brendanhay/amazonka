{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        Pending,
        InProgress,
        Successful,
        Failed,
        Cancelling,
        Cancelled
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

pattern Pending :: InstanceRefreshStatus
pattern Pending = InstanceRefreshStatus' "Pending"

pattern InProgress :: InstanceRefreshStatus
pattern InProgress = InstanceRefreshStatus' "InProgress"

pattern Successful :: InstanceRefreshStatus
pattern Successful = InstanceRefreshStatus' "Successful"

pattern Failed :: InstanceRefreshStatus
pattern Failed = InstanceRefreshStatus' "Failed"

pattern Cancelling :: InstanceRefreshStatus
pattern Cancelling = InstanceRefreshStatus' "Cancelling"

pattern Cancelled :: InstanceRefreshStatus
pattern Cancelled = InstanceRefreshStatus' "Cancelled"

{-# COMPLETE
  Pending,
  InProgress,
  Successful,
  Failed,
  Cancelling,
  Cancelled,
  InstanceRefreshStatus'
  #-}
