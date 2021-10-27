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
-- Module      : Network.AWS.MacieV2.Types.UsageStatisticsFilterComparator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.UsageStatisticsFilterComparator
  ( UsageStatisticsFilterComparator
      ( ..,
        UsageStatisticsFilterComparator_CONTAINS,
        UsageStatisticsFilterComparator_EQ,
        UsageStatisticsFilterComparator_GT,
        UsageStatisticsFilterComparator_GTE,
        UsageStatisticsFilterComparator_LT,
        UsageStatisticsFilterComparator_LTE,
        UsageStatisticsFilterComparator_NE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The operator to use in a condition that filters the results of a query
-- for Amazon Macie account quotas and usage data. Valid values are:
newtype UsageStatisticsFilterComparator = UsageStatisticsFilterComparator'
  { fromUsageStatisticsFilterComparator ::
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

pattern UsageStatisticsFilterComparator_CONTAINS :: UsageStatisticsFilterComparator
pattern UsageStatisticsFilterComparator_CONTAINS = UsageStatisticsFilterComparator' "CONTAINS"

pattern UsageStatisticsFilterComparator_EQ :: UsageStatisticsFilterComparator
pattern UsageStatisticsFilterComparator_EQ = UsageStatisticsFilterComparator' "EQ"

pattern UsageStatisticsFilterComparator_GT :: UsageStatisticsFilterComparator
pattern UsageStatisticsFilterComparator_GT = UsageStatisticsFilterComparator' "GT"

pattern UsageStatisticsFilterComparator_GTE :: UsageStatisticsFilterComparator
pattern UsageStatisticsFilterComparator_GTE = UsageStatisticsFilterComparator' "GTE"

pattern UsageStatisticsFilterComparator_LT :: UsageStatisticsFilterComparator
pattern UsageStatisticsFilterComparator_LT = UsageStatisticsFilterComparator' "LT"

pattern UsageStatisticsFilterComparator_LTE :: UsageStatisticsFilterComparator
pattern UsageStatisticsFilterComparator_LTE = UsageStatisticsFilterComparator' "LTE"

pattern UsageStatisticsFilterComparator_NE :: UsageStatisticsFilterComparator
pattern UsageStatisticsFilterComparator_NE = UsageStatisticsFilterComparator' "NE"

{-# COMPLETE
  UsageStatisticsFilterComparator_CONTAINS,
  UsageStatisticsFilterComparator_EQ,
  UsageStatisticsFilterComparator_GT,
  UsageStatisticsFilterComparator_GTE,
  UsageStatisticsFilterComparator_LT,
  UsageStatisticsFilterComparator_LTE,
  UsageStatisticsFilterComparator_NE,
  UsageStatisticsFilterComparator'
  #-}
