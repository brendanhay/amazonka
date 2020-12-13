{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.MetricType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.MetricType
  ( MetricType
      ( MetricType',
        ASGAverageCPUUtilization,
        ASGAverageNetworkIn,
        ASGAverageNetworkOut,
        ALBRequestCountPerTarget
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MetricType = MetricType' Lude.Text
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

pattern ASGAverageCPUUtilization :: MetricType
pattern ASGAverageCPUUtilization = MetricType' "ASGAverageCPUUtilization"

pattern ASGAverageNetworkIn :: MetricType
pattern ASGAverageNetworkIn = MetricType' "ASGAverageNetworkIn"

pattern ASGAverageNetworkOut :: MetricType
pattern ASGAverageNetworkOut = MetricType' "ASGAverageNetworkOut"

pattern ALBRequestCountPerTarget :: MetricType
pattern ALBRequestCountPerTarget = MetricType' "ALBRequestCountPerTarget"

{-# COMPLETE
  ASGAverageCPUUtilization,
  ASGAverageNetworkIn,
  ASGAverageNetworkOut,
  ALBRequestCountPerTarget,
  MetricType'
  #-}
