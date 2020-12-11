-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ActivityStreamStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ActivityStreamStatus
  ( ActivityStreamStatus
      ( ActivityStreamStatus',
        Started,
        Starting,
        Stopped,
        Stopping
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ActivityStreamStatus = ActivityStreamStatus' Lude.Text
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

pattern Started :: ActivityStreamStatus
pattern Started = ActivityStreamStatus' "started"

pattern Starting :: ActivityStreamStatus
pattern Starting = ActivityStreamStatus' "starting"

pattern Stopped :: ActivityStreamStatus
pattern Stopped = ActivityStreamStatus' "stopped"

pattern Stopping :: ActivityStreamStatus
pattern Stopping = ActivityStreamStatus' "stopping"

{-# COMPLETE
  Started,
  Starting,
  Stopped,
  Stopping,
  ActivityStreamStatus'
  #-}
