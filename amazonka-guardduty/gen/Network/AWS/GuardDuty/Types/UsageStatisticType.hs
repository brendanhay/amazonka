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
-- Module      : Network.AWS.GuardDuty.Types.UsageStatisticType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UsageStatisticType
  ( UsageStatisticType
      ( ..,
        UsageStatisticType_SUM_BY_ACCOUNT,
        UsageStatisticType_SUM_BY_DATA_SOURCE,
        UsageStatisticType_SUM_BY_RESOURCE,
        UsageStatisticType_TOP_RESOURCES
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype UsageStatisticType = UsageStatisticType'
  { fromUsageStatisticType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
