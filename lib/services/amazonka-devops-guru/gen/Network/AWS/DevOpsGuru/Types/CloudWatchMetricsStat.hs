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
-- Module      : Network.AWS.DevOpsGuru.Types.CloudWatchMetricsStat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DevOpsGuru.Types.CloudWatchMetricsStat
  ( CloudWatchMetricsStat
      ( ..,
        CloudWatchMetricsStat_Average,
        CloudWatchMetricsStat_Maximum,
        CloudWatchMetricsStat_Minimum,
        CloudWatchMetricsStat_P50,
        CloudWatchMetricsStat_P90,
        CloudWatchMetricsStat_P99,
        CloudWatchMetricsStat_SampleCount,
        CloudWatchMetricsStat_Sum
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype CloudWatchMetricsStat = CloudWatchMetricsStat'
  { fromCloudWatchMetricsStat ::
      Core.Text
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

pattern CloudWatchMetricsStat_Average :: CloudWatchMetricsStat
pattern CloudWatchMetricsStat_Average = CloudWatchMetricsStat' "Average"

pattern CloudWatchMetricsStat_Maximum :: CloudWatchMetricsStat
pattern CloudWatchMetricsStat_Maximum = CloudWatchMetricsStat' "Maximum"

pattern CloudWatchMetricsStat_Minimum :: CloudWatchMetricsStat
pattern CloudWatchMetricsStat_Minimum = CloudWatchMetricsStat' "Minimum"

pattern CloudWatchMetricsStat_P50 :: CloudWatchMetricsStat
pattern CloudWatchMetricsStat_P50 = CloudWatchMetricsStat' "p50"

pattern CloudWatchMetricsStat_P90 :: CloudWatchMetricsStat
pattern CloudWatchMetricsStat_P90 = CloudWatchMetricsStat' "p90"

pattern CloudWatchMetricsStat_P99 :: CloudWatchMetricsStat
pattern CloudWatchMetricsStat_P99 = CloudWatchMetricsStat' "p99"

pattern CloudWatchMetricsStat_SampleCount :: CloudWatchMetricsStat
pattern CloudWatchMetricsStat_SampleCount = CloudWatchMetricsStat' "SampleCount"

pattern CloudWatchMetricsStat_Sum :: CloudWatchMetricsStat
pattern CloudWatchMetricsStat_Sum = CloudWatchMetricsStat' "Sum"

{-# COMPLETE
  CloudWatchMetricsStat_Average,
  CloudWatchMetricsStat_Maximum,
  CloudWatchMetricsStat_Minimum,
  CloudWatchMetricsStat_P50,
  CloudWatchMetricsStat_P90,
  CloudWatchMetricsStat_P99,
  CloudWatchMetricsStat_SampleCount,
  CloudWatchMetricsStat_Sum,
  CloudWatchMetricsStat'
  #-}
