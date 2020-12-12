{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.UsageStatisticType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.UsageStatisticType
  ( UsageStatisticType
      ( UsageStatisticType',
        SumByAccount,
        SumByDataSource,
        SumByResource,
        TopResources
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype UsageStatisticType = UsageStatisticType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern SumByAccount :: UsageStatisticType
pattern SumByAccount = UsageStatisticType' "SUM_BY_ACCOUNT"

pattern SumByDataSource :: UsageStatisticType
pattern SumByDataSource = UsageStatisticType' "SUM_BY_DATA_SOURCE"

pattern SumByResource :: UsageStatisticType
pattern SumByResource = UsageStatisticType' "SUM_BY_RESOURCE"

pattern TopResources :: UsageStatisticType
pattern TopResources = UsageStatisticType' "TOP_RESOURCES"

{-# COMPLETE
  SumByAccount,
  SumByDataSource,
  SumByResource,
  TopResources,
  UsageStatisticType'
  #-}
