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
-- Module      : Amazonka.AutoScaling.Types.MetricType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.MetricType
  ( MetricType
      ( ..,
        MetricType_ALBRequestCountPerTarget,
        MetricType_ASGAverageCPUUtilization,
        MetricType_ASGAverageNetworkIn,
        MetricType_ASGAverageNetworkOut
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MetricType = MetricType'
  { fromMetricType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
