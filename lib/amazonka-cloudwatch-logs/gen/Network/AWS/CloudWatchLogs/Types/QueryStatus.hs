{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.QueryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.QueryStatus
  ( QueryStatus
      ( QueryStatus',
        Scheduled,
        Running,
        Complete,
        Failed,
        Cancelled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype QueryStatus = QueryStatus' Lude.Text
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

pattern Scheduled :: QueryStatus
pattern Scheduled = QueryStatus' "Scheduled"

pattern Running :: QueryStatus
pattern Running = QueryStatus' "Running"

pattern Complete :: QueryStatus
pattern Complete = QueryStatus' "Complete"

pattern Failed :: QueryStatus
pattern Failed = QueryStatus' "Failed"

pattern Cancelled :: QueryStatus
pattern Cancelled = QueryStatus' "Cancelled"

{-# COMPLETE
  Scheduled,
  Running,
  Complete,
  Failed,
  Cancelled,
  QueryStatus'
  #-}
