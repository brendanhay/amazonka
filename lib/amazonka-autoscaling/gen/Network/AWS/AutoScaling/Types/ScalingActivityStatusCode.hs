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
        Cancelled,
        Failed,
        InProgress,
        MidLifecycleAction,
        PendingSpotBidPlacement,
        PreInService,
        Successful,
        WaitingForELBConnectionDraining,
        WaitingForInstanceId,
        WaitingForInstanceWarmup,
        WaitingForSpotInstanceId,
        WaitingForSpotInstanceRequestId
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

pattern Cancelled :: ScalingActivityStatusCode
pattern Cancelled = ScalingActivityStatusCode' "Cancelled"

pattern Failed :: ScalingActivityStatusCode
pattern Failed = ScalingActivityStatusCode' "Failed"

pattern InProgress :: ScalingActivityStatusCode
pattern InProgress = ScalingActivityStatusCode' "InProgress"

pattern MidLifecycleAction :: ScalingActivityStatusCode
pattern MidLifecycleAction = ScalingActivityStatusCode' "MidLifecycleAction"

pattern PendingSpotBidPlacement :: ScalingActivityStatusCode
pattern PendingSpotBidPlacement = ScalingActivityStatusCode' "PendingSpotBidPlacement"

pattern PreInService :: ScalingActivityStatusCode
pattern PreInService = ScalingActivityStatusCode' "PreInService"

pattern Successful :: ScalingActivityStatusCode
pattern Successful = ScalingActivityStatusCode' "Successful"

pattern WaitingForELBConnectionDraining :: ScalingActivityStatusCode
pattern WaitingForELBConnectionDraining = ScalingActivityStatusCode' "WaitingForELBConnectionDraining"

pattern WaitingForInstanceId :: ScalingActivityStatusCode
pattern WaitingForInstanceId = ScalingActivityStatusCode' "WaitingForInstanceId"

pattern WaitingForInstanceWarmup :: ScalingActivityStatusCode
pattern WaitingForInstanceWarmup = ScalingActivityStatusCode' "WaitingForInstanceWarmup"

pattern WaitingForSpotInstanceId :: ScalingActivityStatusCode
pattern WaitingForSpotInstanceId = ScalingActivityStatusCode' "WaitingForSpotInstanceId"

pattern WaitingForSpotInstanceRequestId :: ScalingActivityStatusCode
pattern WaitingForSpotInstanceRequestId = ScalingActivityStatusCode' "WaitingForSpotInstanceRequestId"

{-# COMPLETE
  Cancelled,
  Failed,
  InProgress,
  MidLifecycleAction,
  PendingSpotBidPlacement,
  PreInService,
  Successful,
  WaitingForELBConnectionDraining,
  WaitingForInstanceId,
  WaitingForInstanceWarmup,
  WaitingForSpotInstanceId,
  WaitingForSpotInstanceRequestId,
  ScalingActivityStatusCode'
  #-}
