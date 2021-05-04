{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype UsageStatisticType = UsageStatisticType'
  { fromUsageStatisticType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
