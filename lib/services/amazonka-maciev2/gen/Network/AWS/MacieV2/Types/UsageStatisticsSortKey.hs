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
-- Module      : Network.AWS.MacieV2.Types.UsageStatisticsSortKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.UsageStatisticsSortKey
  ( UsageStatisticsSortKey
      ( ..,
        UsageStatisticsSortKey_AccountId,
        UsageStatisticsSortKey_FreeTrialStartDate,
        UsageStatisticsSortKey_ServiceLimitValue,
        UsageStatisticsSortKey_Total
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The field to use to sort the results of a query for Amazon Macie account
-- quotas and usage data. Valid values are:
newtype UsageStatisticsSortKey = UsageStatisticsSortKey'
  { fromUsageStatisticsSortKey ::
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
