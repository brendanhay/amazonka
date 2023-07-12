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
-- Module      : Amazonka.GuardDuty.Types.UsageStatisticType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.UsageStatisticType
  ( UsageStatisticType
      ( ..,
        UsageStatisticType_SUM_BY_ACCOUNT,
        UsageStatisticType_SUM_BY_DATA_SOURCE,
        UsageStatisticType_SUM_BY_RESOURCE,
        UsageStatisticType_TOP_RESOURCES
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UsageStatisticType = UsageStatisticType'
  { fromUsageStatisticType ::
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

pattern UsageStatisticType_SUM_BY_ACCOUNT :: UsageStatisticType
pattern UsageStatisticType_SUM_BY_ACCOUNT = UsageStatisticType' "SUM_BY_ACCOUNT"

pattern UsageStatisticType_SUM_BY_DATA_SOURCE :: UsageStatisticType
pattern UsageStatisticType_SUM_BY_DATA_SOURCE = UsageStatisticType' "SUM_BY_DATA_SOURCE"

pattern UsageStatisticType_SUM_BY_RESOURCE :: UsageStatisticType
pattern UsageStatisticType_SUM_BY_RESOURCE = UsageStatisticType' "SUM_BY_RESOURCE"

pattern UsageStatisticType_TOP_RESOURCES :: UsageStatisticType
pattern UsageStatisticType_TOP_RESOURCES = UsageStatisticType' "TOP_RESOURCES"

{-# COMPLETE
  UsageStatisticType_SUM_BY_ACCOUNT,
  UsageStatisticType_SUM_BY_DATA_SOURCE,
  UsageStatisticType_SUM_BY_RESOURCE,
  UsageStatisticType_TOP_RESOURCES,
  UsageStatisticType'
  #-}
