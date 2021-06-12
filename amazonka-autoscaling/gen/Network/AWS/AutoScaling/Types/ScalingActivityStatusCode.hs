{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ScalingActivityStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.ScalingActivityStatusCode
  ( ScalingActivityStatusCode
      ( ..,
        ScalingActivityStatusCode_Cancelled,
        ScalingActivityStatusCode_Failed,
        ScalingActivityStatusCode_InProgress,
        ScalingActivityStatusCode_MidLifecycleAction,
        ScalingActivityStatusCode_PendingSpotBidPlacement,
        ScalingActivityStatusCode_PreInService,
        ScalingActivityStatusCode_Successful,
        ScalingActivityStatusCode_WaitingForELBConnectionDraining,
        ScalingActivityStatusCode_WaitingForInstanceId,
        ScalingActivityStatusCode_WaitingForInstanceWarmup,
        ScalingActivityStatusCode_WaitingForSpotInstanceId,
        ScalingActivityStatusCode_WaitingForSpotInstanceRequestId
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ScalingActivityStatusCode = ScalingActivityStatusCode'
  { fromScalingActivityStatusCode ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ScalingActivityStatusCode_Cancelled :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_Cancelled = ScalingActivityStatusCode' "Cancelled"

pattern ScalingActivityStatusCode_Failed :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_Failed = ScalingActivityStatusCode' "Failed"

pattern ScalingActivityStatusCode_InProgress :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_InProgress = ScalingActivityStatusCode' "InProgress"

pattern ScalingActivityStatusCode_MidLifecycleAction :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_MidLifecycleAction = ScalingActivityStatusCode' "MidLifecycleAction"

pattern ScalingActivityStatusCode_PendingSpotBidPlacement :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_PendingSpotBidPlacement = ScalingActivityStatusCode' "PendingSpotBidPlacement"

pattern ScalingActivityStatusCode_PreInService :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_PreInService = ScalingActivityStatusCode' "PreInService"

pattern ScalingActivityStatusCode_Successful :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_Successful = ScalingActivityStatusCode' "Successful"

pattern ScalingActivityStatusCode_WaitingForELBConnectionDraining :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_WaitingForELBConnectionDraining = ScalingActivityStatusCode' "WaitingForELBConnectionDraining"

pattern ScalingActivityStatusCode_WaitingForInstanceId :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_WaitingForInstanceId = ScalingActivityStatusCode' "WaitingForInstanceId"

pattern ScalingActivityStatusCode_WaitingForInstanceWarmup :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_WaitingForInstanceWarmup = ScalingActivityStatusCode' "WaitingForInstanceWarmup"

pattern ScalingActivityStatusCode_WaitingForSpotInstanceId :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_WaitingForSpotInstanceId = ScalingActivityStatusCode' "WaitingForSpotInstanceId"

pattern ScalingActivityStatusCode_WaitingForSpotInstanceRequestId :: ScalingActivityStatusCode
pattern ScalingActivityStatusCode_WaitingForSpotInstanceRequestId = ScalingActivityStatusCode' "WaitingForSpotInstanceRequestId"

{-# COMPLETE
  ScalingActivityStatusCode_Cancelled,
  ScalingActivityStatusCode_Failed,
  ScalingActivityStatusCode_InProgress,
  ScalingActivityStatusCode_MidLifecycleAction,
  ScalingActivityStatusCode_PendingSpotBidPlacement,
  ScalingActivityStatusCode_PreInService,
  ScalingActivityStatusCode_Successful,
  ScalingActivityStatusCode_WaitingForELBConnectionDraining,
  ScalingActivityStatusCode_WaitingForInstanceId,
  ScalingActivityStatusCode_WaitingForInstanceWarmup,
  ScalingActivityStatusCode_WaitingForSpotInstanceId,
  ScalingActivityStatusCode_WaitingForSpotInstanceRequestId,
  ScalingActivityStatusCode'
  #-}
