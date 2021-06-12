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
-- Module      : Network.AWS.Lightsail.Types.MetricStatistic
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.MetricStatistic
  ( MetricStatistic
      ( ..,
        MetricStatistic_Average,
        MetricStatistic_Maximum,
        MetricStatistic_Minimum,
        MetricStatistic_SampleCount,
        MetricStatistic_Sum
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MetricStatistic = MetricStatistic'
  { fromMetricStatistic ::
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

pattern MetricStatistic_Average :: MetricStatistic
pattern MetricStatistic_Average = MetricStatistic' "Average"

pattern MetricStatistic_Maximum :: MetricStatistic
pattern MetricStatistic_Maximum = MetricStatistic' "Maximum"

pattern MetricStatistic_Minimum :: MetricStatistic
pattern MetricStatistic_Minimum = MetricStatistic' "Minimum"

pattern MetricStatistic_SampleCount :: MetricStatistic
pattern MetricStatistic_SampleCount = MetricStatistic' "SampleCount"

pattern MetricStatistic_Sum :: MetricStatistic
pattern MetricStatistic_Sum = MetricStatistic' "Sum"

{-# COMPLETE
  MetricStatistic_Average,
  MetricStatistic_Maximum,
  MetricStatistic_Minimum,
  MetricStatistic_SampleCount,
  MetricStatistic_Sum,
  MetricStatistic'
  #-}
