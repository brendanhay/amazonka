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
-- Module      : Amazonka.MacieV2.Types.UsageStatisticsFilterComparator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.UsageStatisticsFilterComparator
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The operator to use in a condition that filters the results of a query
-- for Amazon Macie account quotas and usage data. Valid values are:
newtype UsageStatisticsFilterComparator = UsageStatisticsFilterComparator'
  { fromUsageStatisticsFilterComparator ::
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
