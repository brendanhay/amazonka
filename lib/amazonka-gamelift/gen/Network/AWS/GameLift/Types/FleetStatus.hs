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
        FleetStatusNew,
        FleetStatusDownloading,
        FleetStatusValidating,
        FleetStatusBuilding,
        FleetStatusActivating,
        FleetStatusActive,
        FleetStatusDeleting,
        FleetStatusError,
        FleetStatusTerminated,
        fromFleetStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype FleetStatus = FleetStatus' {fromFleetStatus :: Core.Text}
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

pattern FleetStatusNew :: FleetStatus
pattern FleetStatusNew = FleetStatus' "NEW"

pattern FleetStatusDownloading :: FleetStatus
pattern FleetStatusDownloading = FleetStatus' "DOWNLOADING"

pattern FleetStatusValidating :: FleetStatus
pattern FleetStatusValidating = FleetStatus' "VALIDATING"

pattern FleetStatusBuilding :: FleetStatus
pattern FleetStatusBuilding = FleetStatus' "BUILDING"

pattern FleetStatusActivating :: FleetStatus
pattern FleetStatusActivating = FleetStatus' "ACTIVATING"

pattern FleetStatusActive :: FleetStatus
pattern FleetStatusActive = FleetStatus' "ACTIVE"

pattern FleetStatusDeleting :: FleetStatus
pattern FleetStatusDeleting = FleetStatus' "DELETING"

pattern FleetStatusError :: FleetStatus
pattern FleetStatusError = FleetStatus' "ERROR"

pattern FleetStatusTerminated :: FleetStatus
pattern FleetStatusTerminated = FleetStatus' "TERMINATED"

{-# COMPLETE
  FleetStatusNew,
  FleetStatusDownloading,
  FleetStatusValidating,
  FleetStatusBuilding,
  FleetStatusActivating,
  FleetStatusActive,
  FleetStatusDeleting,
  FleetStatusError,
  FleetStatusTerminated,
  FleetStatus'
  #-}
