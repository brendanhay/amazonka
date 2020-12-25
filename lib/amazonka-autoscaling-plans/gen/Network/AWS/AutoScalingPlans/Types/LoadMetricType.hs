{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        LoadMetricTypeASGTotalCPUUtilization,
        LoadMetricTypeASGTotalNetworkIn,
        LoadMetricTypeASGTotalNetworkOut,
        LoadMetricTypeALBTargetGroupRequestCount,
        fromLoadMetricType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype LoadMetricType = LoadMetricType'
  { fromLoadMetricType ::
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

pattern LoadMetricTypeASGTotalCPUUtilization :: LoadMetricType
pattern LoadMetricTypeASGTotalCPUUtilization = LoadMetricType' "ASGTotalCPUUtilization"

pattern LoadMetricTypeASGTotalNetworkIn :: LoadMetricType
pattern LoadMetricTypeASGTotalNetworkIn = LoadMetricType' "ASGTotalNetworkIn"

pattern LoadMetricTypeASGTotalNetworkOut :: LoadMetricType
pattern LoadMetricTypeASGTotalNetworkOut = LoadMetricType' "ASGTotalNetworkOut"

pattern LoadMetricTypeALBTargetGroupRequestCount :: LoadMetricType
pattern LoadMetricTypeALBTargetGroupRequestCount = LoadMetricType' "ALBTargetGroupRequestCount"

{-# COMPLETE
  LoadMetricTypeASGTotalCPUUtilization,
  LoadMetricTypeASGTotalNetworkIn,
  LoadMetricTypeASGTotalNetworkOut,
  LoadMetricTypeALBTargetGroupRequestCount,
  LoadMetricType'
  #-}
