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
-- Module      : Network.AWS.MacieV2.Types.UsageStatisticsFilterKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.UsageStatisticsFilterKey
  ( UsageStatisticsFilterKey
      ( ..,
        UsageStatisticsFilterKey_AccountId,
        UsageStatisticsFilterKey_FreeTrialStartDate,
        UsageStatisticsFilterKey_ServiceLimit,
        UsageStatisticsFilterKey_Total
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The field to use in a condition that filters the results of a query for
-- Amazon Macie account quotas and usage data. Valid values are:
newtype UsageStatisticsFilterKey = UsageStatisticsFilterKey'
  { fromUsageStatisticsFilterKey ::
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
