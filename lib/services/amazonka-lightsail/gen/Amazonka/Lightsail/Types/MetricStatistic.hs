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
-- Module      : Amazonka.Lightsail.Types.MetricStatistic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.MetricStatistic
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MetricStatistic = MetricStatistic'
  { fromMetricStatistic ::
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
