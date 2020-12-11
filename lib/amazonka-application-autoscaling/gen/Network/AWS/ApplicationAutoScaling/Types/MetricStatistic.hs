-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.MetricStatistic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.MetricStatistic
  ( MetricStatistic
      ( MetricStatistic',
        Average,
        Maximum,
        Minimum,
        SampleCount,
        Sum
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MetricStatistic = MetricStatistic' Lude.Text
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

pattern Average :: MetricStatistic
pattern Average = MetricStatistic' "Average"

pattern Maximum :: MetricStatistic
pattern Maximum = MetricStatistic' "Maximum"

pattern Minimum :: MetricStatistic
pattern Minimum = MetricStatistic' "Minimum"

pattern SampleCount :: MetricStatistic
pattern SampleCount = MetricStatistic' "SampleCount"

pattern Sum :: MetricStatistic
pattern Sum = MetricStatistic' "Sum"

{-# COMPLETE
  Average,
  Maximum,
  Minimum,
  SampleCount,
  Sum,
  MetricStatistic'
  #-}
