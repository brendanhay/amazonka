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
-- Module      : Network.AWS.AutoScaling.Types.MetricType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.MetricType
  ( MetricType
      ( ..,
        MetricType_ALBRequestCountPerTarget,
        MetricType_ASGAverageCPUUtilization,
        MetricType_ASGAverageNetworkIn,
        MetricType_ASGAverageNetworkOut
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MetricType = MetricType'
  { fromMetricType ::
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

pattern MetricType_ALBRequestCountPerTarget :: MetricType
pattern MetricType_ALBRequestCountPerTarget = MetricType' "ALBRequestCountPerTarget"

pattern MetricType_ASGAverageCPUUtilization :: MetricType
pattern MetricType_ASGAverageCPUUtilization = MetricType' "ASGAverageCPUUtilization"

pattern MetricType_ASGAverageNetworkIn :: MetricType
pattern MetricType_ASGAverageNetworkIn = MetricType' "ASGAverageNetworkIn"

pattern MetricType_ASGAverageNetworkOut :: MetricType
pattern MetricType_ASGAverageNetworkOut = MetricType' "ASGAverageNetworkOut"

{-# COMPLETE
  MetricType_ALBRequestCountPerTarget,
  MetricType_ASGAverageCPUUtilization,
  MetricType_ASGAverageNetworkIn,
  MetricType_ASGAverageNetworkOut,
  MetricType'
  #-}
