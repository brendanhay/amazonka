{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        FSNew,
        FSDownloading,
        FSValidating,
        FSBuilding,
        FSActivating,
        FSActive,
        FSDeleting,
        FSError,
        FSTerminated
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

pattern FSNew :: FleetStatus
pattern FSNew = FleetStatus' "NEW"

pattern FSDownloading :: FleetStatus
pattern FSDownloading = FleetStatus' "DOWNLOADING"

pattern FSValidating :: FleetStatus
pattern FSValidating = FleetStatus' "VALIDATING"

pattern FSBuilding :: FleetStatus
pattern FSBuilding = FleetStatus' "BUILDING"

pattern FSActivating :: FleetStatus
pattern FSActivating = FleetStatus' "ACTIVATING"

pattern FSActive :: FleetStatus
pattern FSActive = FleetStatus' "ACTIVE"

pattern FSDeleting :: FleetStatus
pattern FSDeleting = FleetStatus' "DELETING"

pattern FSError :: FleetStatus
pattern FSError = FleetStatus' "ERROR"

pattern FSTerminated :: FleetStatus
pattern FSTerminated = FleetStatus' "TERMINATED"

{-# COMPLETE
  FSNew,
  FSDownloading,
  FSValidating,
  FSBuilding,
  FSActivating,
  FSActive,
  FSDeleting,
  FSError,
  FSTerminated,
  FleetStatus'
  #-}
