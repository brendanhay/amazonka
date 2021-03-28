{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.Types.HsmStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudHSM.Types.HsmStatus
  ( HsmStatus
    ( HsmStatus'
    , HsmStatusPending
    , HsmStatusRunning
    , HsmStatusUpdating
    , HsmStatusSuspended
    , HsmStatusTerminating
    , HsmStatusTerminated
    , HsmStatusDegraded
    , fromHsmStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype HsmStatus = HsmStatus'{fromHsmStatus :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern HsmStatusPending :: HsmStatus
pattern HsmStatusPending = HsmStatus' "PENDING"

pattern HsmStatusRunning :: HsmStatus
pattern HsmStatusRunning = HsmStatus' "RUNNING"

pattern HsmStatusUpdating :: HsmStatus
pattern HsmStatusUpdating = HsmStatus' "UPDATING"

pattern HsmStatusSuspended :: HsmStatus
pattern HsmStatusSuspended = HsmStatus' "SUSPENDED"

pattern HsmStatusTerminating :: HsmStatus
pattern HsmStatusTerminating = HsmStatus' "TERMINATING"

pattern HsmStatusTerminated :: HsmStatus
pattern HsmStatusTerminated = HsmStatus' "TERMINATED"

pattern HsmStatusDegraded :: HsmStatus
pattern HsmStatusDegraded = HsmStatus' "DEGRADED"

{-# COMPLETE 
  HsmStatusPending,

  HsmStatusRunning,

  HsmStatusUpdating,

  HsmStatusSuspended,

  HsmStatusTerminating,

  HsmStatusTerminated,

  HsmStatusDegraded,
  HsmStatus'
  #-}
