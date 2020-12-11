-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.LoadMetricType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.LoadMetricType
  ( LoadMetricType
      ( LoadMetricType',
        ALBTargetGroupRequestCount,
        ASGTotalCPUUtilization,
        ASGTotalNetworkIn,
        ASGTotalNetworkOut
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LoadMetricType = LoadMetricType' Lude.Text
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

pattern ALBTargetGroupRequestCount :: LoadMetricType
pattern ALBTargetGroupRequestCount = LoadMetricType' "ALBTargetGroupRequestCount"

pattern ASGTotalCPUUtilization :: LoadMetricType
pattern ASGTotalCPUUtilization = LoadMetricType' "ASGTotalCPUUtilization"

pattern ASGTotalNetworkIn :: LoadMetricType
pattern ASGTotalNetworkIn = LoadMetricType' "ASGTotalNetworkIn"

pattern ASGTotalNetworkOut :: LoadMetricType
pattern ASGTotalNetworkOut = LoadMetricType' "ASGTotalNetworkOut"

{-# COMPLETE
  ALBTargetGroupRequestCount,
  ASGTotalCPUUtilization,
  ASGTotalNetworkIn,
  ASGTotalNetworkOut,
  LoadMetricType'
  #-}
