{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.MetricStatistic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.MetricStatistic
  ( MetricStatistic
      ( MetricStatistic',
        Minimum,
        Maximum,
        Sum,
        Average,
        SampleCount
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

pattern Minimum :: MetricStatistic
pattern Minimum = MetricStatistic' "Minimum"

pattern Maximum :: MetricStatistic
pattern Maximum = MetricStatistic' "Maximum"

pattern Sum :: MetricStatistic
pattern Sum = MetricStatistic' "Sum"

pattern Average :: MetricStatistic
pattern Average = MetricStatistic' "Average"

pattern SampleCount :: MetricStatistic
pattern SampleCount = MetricStatistic' "SampleCount"

{-# COMPLETE
  Minimum,
  Maximum,
  Sum,
  Average,
  SampleCount,
  MetricStatistic'
  #-}
