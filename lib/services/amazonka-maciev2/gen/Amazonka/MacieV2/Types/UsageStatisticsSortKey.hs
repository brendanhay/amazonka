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
-- Module      : Amazonka.MacieV2.Types.UsageStatisticsSortKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.UsageStatisticsSortKey
  ( UsageStatisticsSortKey
      ( ..,
        UsageStatisticsSortKey_AccountId,
        UsageStatisticsSortKey_FreeTrialStartDate,
        UsageStatisticsSortKey_ServiceLimitValue,
        UsageStatisticsSortKey_Total
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The field to use to sort the results of a query for Amazon Macie account
-- quotas and usage data. Valid values are:
newtype UsageStatisticsSortKey = UsageStatisticsSortKey'
  { fromUsageStatisticsSortKey ::
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

pattern UsageStatisticsSortKey_AccountId :: UsageStatisticsSortKey
pattern UsageStatisticsSortKey_AccountId = UsageStatisticsSortKey' "accountId"

pattern UsageStatisticsSortKey_FreeTrialStartDate :: UsageStatisticsSortKey
pattern UsageStatisticsSortKey_FreeTrialStartDate = UsageStatisticsSortKey' "freeTrialStartDate"

pattern UsageStatisticsSortKey_ServiceLimitValue :: UsageStatisticsSortKey
pattern UsageStatisticsSortKey_ServiceLimitValue = UsageStatisticsSortKey' "serviceLimitValue"

pattern UsageStatisticsSortKey_Total :: UsageStatisticsSortKey
pattern UsageStatisticsSortKey_Total = UsageStatisticsSortKey' "total"

{-# COMPLETE
  UsageStatisticsSortKey_AccountId,
  UsageStatisticsSortKey_FreeTrialStartDate,
  UsageStatisticsSortKey_ServiceLimitValue,
  UsageStatisticsSortKey_Total,
  UsageStatisticsSortKey'
  #-}
