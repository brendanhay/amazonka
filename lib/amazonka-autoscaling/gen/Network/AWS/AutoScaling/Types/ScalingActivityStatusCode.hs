{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ScalingActivityStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.ScalingActivityStatusCode
  ( ScalingActivityStatusCode
      ( ScalingActivityStatusCode',
        ScalingActivityStatusCodePendingSpotBidPlacement,
        ScalingActivityStatusCodeWaitingForSpotInstanceRequestId,
        ScalingActivityStatusCodeWaitingForSpotInstanceId,
        ScalingActivityStatusCodeWaitingForInstanceId,
        ScalingActivityStatusCodePreInService,
        ScalingActivityStatusCodeInProgress,
        ScalingActivityStatusCodeWaitingForELBConnectionDraining,
        ScalingActivityStatusCodeMidLifecycleAction,
        ScalingActivityStatusCodeWaitingForInstanceWarmup,
        ScalingActivityStatusCodeSuccessful,
        ScalingActivityStatusCodeFailed,
        ScalingActivityStatusCodeCancelled,
        fromScalingActivityStatusCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ScalingActivityStatusCode = ScalingActivityStatusCode'
  { fromScalingActivityStatusCode ::
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

pattern ScalingActivityStatusCodePendingSpotBidPlacement :: ScalingActivityStatusCode
pattern ScalingActivityStatusCodePendingSpotBidPlacement = ScalingActivityStatusCode' "PendingSpotBidPlacement"

pattern ScalingActivityStatusCodeWaitingForSpotInstanceRequestId :: ScalingActivityStatusCode
pattern ScalingActivityStatusCodeWaitingForSpotInstanceRequestId = ScalingActivityStatusCode' "WaitingForSpotInstanceRequestId"

pattern ScalingActivityStatusCodeWaitingForSpotInstanceId :: ScalingActivityStatusCode
pattern ScalingActivityStatusCodeWaitingForSpotInstanceId = ScalingActivityStatusCode' "WaitingForSpotInstanceId"

pattern ScalingActivityStatusCodeWaitingForInstanceId :: ScalingActivityStatusCode
pattern ScalingActivityStatusCodeWaitingForInstanceId = ScalingActivityStatusCode' "WaitingForInstanceId"

pattern ScalingActivityStatusCodePreInService :: ScalingActivityStatusCode
pattern ScalingActivityStatusCodePreInService = ScalingActivityStatusCode' "PreInService"

pattern ScalingActivityStatusCodeInProgress :: ScalingActivityStatusCode
pattern ScalingActivityStatusCodeInProgress = ScalingActivityStatusCode' "InProgress"

pattern ScalingActivityStatusCodeWaitingForELBConnectionDraining :: ScalingActivityStatusCode
pattern ScalingActivityStatusCodeWaitingForELBConnectionDraining = ScalingActivityStatusCode' "WaitingForELBConnectionDraining"

pattern ScalingActivityStatusCodeMidLifecycleAction :: ScalingActivityStatusCode
pattern ScalingActivityStatusCodeMidLifecycleAction = ScalingActivityStatusCode' "MidLifecycleAction"

pattern ScalingActivityStatusCodeWaitingForInstanceWarmup :: ScalingActivityStatusCode
pattern ScalingActivityStatusCodeWaitingForInstanceWarmup = ScalingActivityStatusCode' "WaitingForInstanceWarmup"

pattern ScalingActivityStatusCodeSuccessful :: ScalingActivityStatusCode
pattern ScalingActivityStatusCodeSuccessful = ScalingActivityStatusCode' "Successful"

pattern ScalingActivityStatusCodeFailed :: ScalingActivityStatusCode
pattern ScalingActivityStatusCodeFailed = ScalingActivityStatusCode' "Failed"

pattern ScalingActivityStatusCodeCancelled :: ScalingActivityStatusCode
pattern ScalingActivityStatusCodeCancelled = ScalingActivityStatusCode' "Cancelled"

{-# COMPLETE
  ScalingActivityStatusCodePendingSpotBidPlacement,
  ScalingActivityStatusCodeWaitingForSpotInstanceRequestId,
  ScalingActivityStatusCodeWaitingForSpotInstanceId,
  ScalingActivityStatusCodeWaitingForInstanceId,
  ScalingActivityStatusCodePreInService,
  ScalingActivityStatusCodeInProgress,
  ScalingActivityStatusCodeWaitingForELBConnectionDraining,
  ScalingActivityStatusCodeMidLifecycleAction,
  ScalingActivityStatusCodeWaitingForInstanceWarmup,
  ScalingActivityStatusCodeSuccessful,
  ScalingActivityStatusCodeFailed,
  ScalingActivityStatusCodeCancelled,
  ScalingActivityStatusCode'
  #-}
