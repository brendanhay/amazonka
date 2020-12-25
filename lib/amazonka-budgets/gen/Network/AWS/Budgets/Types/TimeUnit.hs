{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.TimeUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.TimeUnit
  ( TimeUnit
      ( TimeUnit',
        TimeUnitDaily,
        TimeUnitMonthly,
        TimeUnitQuarterly,
        TimeUnitAnnually,
        fromTimeUnit
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The time unit of the budget, such as MONTHLY or QUARTERLY.
newtype TimeUnit = TimeUnit' {fromTimeUnit :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern TimeUnitDaily :: TimeUnit
pattern TimeUnitDaily = TimeUnit' "DAILY"

pattern TimeUnitMonthly :: TimeUnit
pattern TimeUnitMonthly = TimeUnit' "MONTHLY"

pattern TimeUnitQuarterly :: TimeUnit
pattern TimeUnitQuarterly = TimeUnit' "QUARTERLY"

pattern TimeUnitAnnually :: TimeUnit
pattern TimeUnitAnnually = TimeUnit' "ANNUALLY"

{-# COMPLETE
  TimeUnitDaily,
  TimeUnitMonthly,
  TimeUnitQuarterly,
  TimeUnitAnnually,
  TimeUnit'
  #-}
