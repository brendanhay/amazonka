{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ActivityStreamStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.ActivityStreamStatus
  ( ActivityStreamStatus
    ( ActivityStreamStatus'
    , ActivityStreamStatusStopped
    , ActivityStreamStatusStarting
    , ActivityStreamStatusStarted
    , ActivityStreamStatusStopping
    , fromActivityStreamStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ActivityStreamStatus = ActivityStreamStatus'{fromActivityStreamStatus
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern ActivityStreamStatusStopped :: ActivityStreamStatus
pattern ActivityStreamStatusStopped = ActivityStreamStatus' "stopped"

pattern ActivityStreamStatusStarting :: ActivityStreamStatus
pattern ActivityStreamStatusStarting = ActivityStreamStatus' "starting"

pattern ActivityStreamStatusStarted :: ActivityStreamStatus
pattern ActivityStreamStatusStarted = ActivityStreamStatus' "started"

pattern ActivityStreamStatusStopping :: ActivityStreamStatus
pattern ActivityStreamStatusStopping = ActivityStreamStatus' "stopping"

{-# COMPLETE 
  ActivityStreamStatusStopped,

  ActivityStreamStatusStarting,

  ActivityStreamStatusStarted,

  ActivityStreamStatusStopping,
  ActivityStreamStatus'
  #-}
