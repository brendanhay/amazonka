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
-- Module      : Amazonka.MacieV2.Types.UsageStatisticsFilterKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.UsageStatisticsFilterKey
  ( UsageStatisticsFilterKey
      ( ..,
        UsageStatisticsFilterKey_AccountId,
        UsageStatisticsFilterKey_FreeTrialStartDate,
        UsageStatisticsFilterKey_ServiceLimit,
        UsageStatisticsFilterKey_Total
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The field to use in a condition that filters the results of a query for
-- Amazon Macie account quotas and usage data. Valid values are:
newtype UsageStatisticsFilterKey = UsageStatisticsFilterKey'
  { fromUsageStatisticsFilterKey ::
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

pattern UsageStatisticsFilterKey_AccountId :: UsageStatisticsFilterKey
pattern UsageStatisticsFilterKey_AccountId = UsageStatisticsFilterKey' "accountId"

pattern UsageStatisticsFilterKey_FreeTrialStartDate :: UsageStatisticsFilterKey
pattern UsageStatisticsFilterKey_FreeTrialStartDate = UsageStatisticsFilterKey' "freeTrialStartDate"

pattern UsageStatisticsFilterKey_ServiceLimit :: UsageStatisticsFilterKey
pattern UsageStatisticsFilterKey_ServiceLimit = UsageStatisticsFilterKey' "serviceLimit"

pattern UsageStatisticsFilterKey_Total :: UsageStatisticsFilterKey
pattern UsageStatisticsFilterKey_Total = UsageStatisticsFilterKey' "total"

{-# COMPLETE
  UsageStatisticsFilterKey_AccountId,
  UsageStatisticsFilterKey_FreeTrialStartDate,
  UsageStatisticsFilterKey_ServiceLimit,
  UsageStatisticsFilterKey_Total,
  UsageStatisticsFilterKey'
  #-}
