{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Statistic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.Statistic
  ( Statistic
    ( Statistic'
    , StatisticSampleCount
    , StatisticAverage
    , StatisticSum
    , StatisticMinimum
    , StatisticMaximum
    , fromStatistic
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Statistic = Statistic'{fromStatistic :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern StatisticSampleCount :: Statistic
pattern StatisticSampleCount = Statistic' "SAMPLE_COUNT"

pattern StatisticAverage :: Statistic
pattern StatisticAverage = Statistic' "AVERAGE"

pattern StatisticSum :: Statistic
pattern StatisticSum = Statistic' "SUM"

pattern StatisticMinimum :: Statistic
pattern StatisticMinimum = Statistic' "MINIMUM"

pattern StatisticMaximum :: Statistic
pattern StatisticMaximum = Statistic' "MAXIMUM"

{-# COMPLETE 
  StatisticSampleCount,

  StatisticAverage,

  StatisticSum,

  StatisticMinimum,

  StatisticMaximum,
  Statistic'
  #-}
