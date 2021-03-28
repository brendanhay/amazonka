{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UsageStatisticType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.UsageStatisticType
  ( UsageStatisticType
    ( UsageStatisticType'
    , UsageStatisticTypeSumByAccount
    , UsageStatisticTypeSumByDataSource
    , UsageStatisticTypeSumByResource
    , UsageStatisticTypeTopResources
    , fromUsageStatisticType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype UsageStatisticType = UsageStatisticType'{fromUsageStatisticType
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern UsageStatisticTypeSumByAccount :: UsageStatisticType
pattern UsageStatisticTypeSumByAccount = UsageStatisticType' "SUM_BY_ACCOUNT"

pattern UsageStatisticTypeSumByDataSource :: UsageStatisticType
pattern UsageStatisticTypeSumByDataSource = UsageStatisticType' "SUM_BY_DATA_SOURCE"

pattern UsageStatisticTypeSumByResource :: UsageStatisticType
pattern UsageStatisticTypeSumByResource = UsageStatisticType' "SUM_BY_RESOURCE"

pattern UsageStatisticTypeTopResources :: UsageStatisticType
pattern UsageStatisticTypeTopResources = UsageStatisticType' "TOP_RESOURCES"

{-# COMPLETE 
  UsageStatisticTypeSumByAccount,

  UsageStatisticTypeSumByDataSource,

  UsageStatisticTypeSumByResource,

  UsageStatisticTypeTopResources,
  UsageStatisticType'
  #-}
