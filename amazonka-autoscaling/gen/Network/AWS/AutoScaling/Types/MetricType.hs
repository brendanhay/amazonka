{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype MetricType = MetricType'
  { fromMetricType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
