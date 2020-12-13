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
        SASCPendingSpotBidPlacement,
        SASCWaitingForSpotInstanceRequestId,
        SASCWaitingForSpotInstanceId,
        SASCWaitingForInstanceId,
        SASCPreInService,
        SASCInProgress,
        SASCWaitingForELBConnectionDraining,
        SASCMidLifecycleAction,
        SASCWaitingForInstanceWarmup,
        SASCSuccessful,
        SASCFailed,
        SASCCancelled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ScalingActivityStatusCode = ScalingActivityStatusCode' Lude.Text
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

pattern SASCPendingSpotBidPlacement :: ScalingActivityStatusCode
pattern SASCPendingSpotBidPlacement = ScalingActivityStatusCode' "PendingSpotBidPlacement"

pattern SASCWaitingForSpotInstanceRequestId :: ScalingActivityStatusCode
pattern SASCWaitingForSpotInstanceRequestId = ScalingActivityStatusCode' "WaitingForSpotInstanceRequestId"

pattern SASCWaitingForSpotInstanceId :: ScalingActivityStatusCode
pattern SASCWaitingForSpotInstanceId = ScalingActivityStatusCode' "WaitingForSpotInstanceId"

pattern SASCWaitingForInstanceId :: ScalingActivityStatusCode
pattern SASCWaitingForInstanceId = ScalingActivityStatusCode' "WaitingForInstanceId"

pattern SASCPreInService :: ScalingActivityStatusCode
pattern SASCPreInService = ScalingActivityStatusCode' "PreInService"

pattern SASCInProgress :: ScalingActivityStatusCode
pattern SASCInProgress = ScalingActivityStatusCode' "InProgress"

pattern SASCWaitingForELBConnectionDraining :: ScalingActivityStatusCode
pattern SASCWaitingForELBConnectionDraining = ScalingActivityStatusCode' "WaitingForELBConnectionDraining"

pattern SASCMidLifecycleAction :: ScalingActivityStatusCode
pattern SASCMidLifecycleAction = ScalingActivityStatusCode' "MidLifecycleAction"

pattern SASCWaitingForInstanceWarmup :: ScalingActivityStatusCode
pattern SASCWaitingForInstanceWarmup = ScalingActivityStatusCode' "WaitingForInstanceWarmup"

pattern SASCSuccessful :: ScalingActivityStatusCode
pattern SASCSuccessful = ScalingActivityStatusCode' "Successful"

pattern SASCFailed :: ScalingActivityStatusCode
pattern SASCFailed = ScalingActivityStatusCode' "Failed"

pattern SASCCancelled :: ScalingActivityStatusCode
pattern SASCCancelled = ScalingActivityStatusCode' "Cancelled"

{-# COMPLETE
  SASCPendingSpotBidPlacement,
  SASCWaitingForSpotInstanceRequestId,
  SASCWaitingForSpotInstanceId,
  SASCWaitingForInstanceId,
  SASCPreInService,
  SASCInProgress,
  SASCWaitingForELBConnectionDraining,
  SASCMidLifecycleAction,
  SASCWaitingForInstanceWarmup,
  SASCSuccessful,
  SASCFailed,
  SASCCancelled,
  ScalingActivityStatusCode'
  #-}
