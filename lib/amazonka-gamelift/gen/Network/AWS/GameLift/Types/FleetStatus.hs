-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.FleetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.FleetStatus
  ( FleetStatus
      ( FleetStatus',
        FSActivating,
        FSActive,
        FSBuilding,
        FSDeleting,
        FSDownloading,
        FSError,
        FSNew,
        FSTerminated,
        FSValidating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FleetStatus = FleetStatus' Lude.Text
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

pattern FSActivating :: FleetStatus
pattern FSActivating = FleetStatus' "ACTIVATING"

pattern FSActive :: FleetStatus
pattern FSActive = FleetStatus' "ACTIVE"

pattern FSBuilding :: FleetStatus
pattern FSBuilding = FleetStatus' "BUILDING"

pattern FSDeleting :: FleetStatus
pattern FSDeleting = FleetStatus' "DELETING"

pattern FSDownloading :: FleetStatus
pattern FSDownloading = FleetStatus' "DOWNLOADING"

pattern FSError :: FleetStatus
pattern FSError = FleetStatus' "ERROR"

pattern FSNew :: FleetStatus
pattern FSNew = FleetStatus' "NEW"

pattern FSTerminated :: FleetStatus
pattern FSTerminated = FleetStatus' "TERMINATED"

pattern FSValidating :: FleetStatus
pattern FSValidating = FleetStatus' "VALIDATING"

{-# COMPLETE
  FSActivating,
  FSActive,
  FSBuilding,
  FSDeleting,
  FSDownloading,
  FSError,
  FSNew,
  FSTerminated,
  FSValidating,
  FleetStatus'
  #-}
