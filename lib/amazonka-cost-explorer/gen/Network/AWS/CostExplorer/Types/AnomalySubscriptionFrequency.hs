{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.AnomalySubscriptionFrequency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AnomalySubscriptionFrequency
  ( AnomalySubscriptionFrequency
      ( AnomalySubscriptionFrequency',
        AnomalySubscriptionFrequencyDaily,
        AnomalySubscriptionFrequencyImmediate,
        AnomalySubscriptionFrequencyWeekly,
        fromAnomalySubscriptionFrequency
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AnomalySubscriptionFrequency = AnomalySubscriptionFrequency'
  { fromAnomalySubscriptionFrequency ::
      Core.Text
  }
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

pattern AnomalySubscriptionFrequencyDaily :: AnomalySubscriptionFrequency
pattern AnomalySubscriptionFrequencyDaily = AnomalySubscriptionFrequency' "DAILY"

pattern AnomalySubscriptionFrequencyImmediate :: AnomalySubscriptionFrequency
pattern AnomalySubscriptionFrequencyImmediate = AnomalySubscriptionFrequency' "IMMEDIATE"

pattern AnomalySubscriptionFrequencyWeekly :: AnomalySubscriptionFrequency
pattern AnomalySubscriptionFrequencyWeekly = AnomalySubscriptionFrequency' "WEEKLY"

{-# COMPLETE
  AnomalySubscriptionFrequencyDaily,
  AnomalySubscriptionFrequencyImmediate,
  AnomalySubscriptionFrequencyWeekly,
  AnomalySubscriptionFrequency'
  #-}
