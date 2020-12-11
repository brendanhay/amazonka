-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.FleetState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.FleetState
  ( FleetState
      ( FleetState',
        Running,
        Starting,
        Stopped,
        Stopping
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FleetState = FleetState' Lude.Text
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

pattern Running :: FleetState
pattern Running = FleetState' "RUNNING"

pattern Starting :: FleetState
pattern Starting = FleetState' "STARTING"

pattern Stopped :: FleetState
pattern Stopped = FleetState' "STOPPED"

pattern Stopping :: FleetState
pattern Stopping = FleetState' "STOPPING"

{-# COMPLETE
  Running,
  Starting,
  Stopped,
  Stopping,
  FleetState'
  #-}
