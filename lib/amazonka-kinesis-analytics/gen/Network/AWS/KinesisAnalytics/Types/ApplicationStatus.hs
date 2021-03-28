{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.ApplicationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.ApplicationStatus
  ( ApplicationStatus
    ( ApplicationStatus'
    , ApplicationStatusDeleting
    , ApplicationStatusStarting
    , ApplicationStatusStopping
    , ApplicationStatusReady
    , ApplicationStatusRunning
    , ApplicationStatusUpdating
    , fromApplicationStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ApplicationStatus = ApplicationStatus'{fromApplicationStatus
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern ApplicationStatusDeleting :: ApplicationStatus
pattern ApplicationStatusDeleting = ApplicationStatus' "DELETING"

pattern ApplicationStatusStarting :: ApplicationStatus
pattern ApplicationStatusStarting = ApplicationStatus' "STARTING"

pattern ApplicationStatusStopping :: ApplicationStatus
pattern ApplicationStatusStopping = ApplicationStatus' "STOPPING"

pattern ApplicationStatusReady :: ApplicationStatus
pattern ApplicationStatusReady = ApplicationStatus' "READY"

pattern ApplicationStatusRunning :: ApplicationStatus
pattern ApplicationStatusRunning = ApplicationStatus' "RUNNING"

pattern ApplicationStatusUpdating :: ApplicationStatus
pattern ApplicationStatusUpdating = ApplicationStatus' "UPDATING"

{-# COMPLETE 
  ApplicationStatusDeleting,

  ApplicationStatusStarting,

  ApplicationStatusStopping,

  ApplicationStatusReady,

  ApplicationStatusRunning,

  ApplicationStatusUpdating,
  ApplicationStatus'
  #-}
