{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FleetActivityStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FleetActivityStatus
  ( FleetActivityStatus
      ( FleetActivityStatus',
        FleetActivityStatusError,
        FleetActivityStatusPendingFulfillment,
        FleetActivityStatusPendingTermination,
        FleetActivityStatusFulfilled,
        fromFleetActivityStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype FleetActivityStatus = FleetActivityStatus'
  { fromFleetActivityStatus ::
      Core.Text
  }
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

pattern FleetActivityStatusError :: FleetActivityStatus
pattern FleetActivityStatusError = FleetActivityStatus' "error"

pattern FleetActivityStatusPendingFulfillment :: FleetActivityStatus
pattern FleetActivityStatusPendingFulfillment = FleetActivityStatus' "pending_fulfillment"

pattern FleetActivityStatusPendingTermination :: FleetActivityStatus
pattern FleetActivityStatusPendingTermination = FleetActivityStatus' "pending_termination"

pattern FleetActivityStatusFulfilled :: FleetActivityStatus
pattern FleetActivityStatusFulfilled = FleetActivityStatus' "fulfilled"

{-# COMPLETE
  FleetActivityStatusError,
  FleetActivityStatusPendingFulfillment,
  FleetActivityStatusPendingTermination,
  FleetActivityStatusFulfilled,
  FleetActivityStatus'
  #-}
