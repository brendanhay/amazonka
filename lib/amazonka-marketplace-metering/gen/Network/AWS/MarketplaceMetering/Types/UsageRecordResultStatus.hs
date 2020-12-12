{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.Types.UsageRecordResultStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceMetering.Types.UsageRecordResultStatus
  ( UsageRecordResultStatus
      ( UsageRecordResultStatus',
        CustomerNotSubscribed,
        DuplicateRecord,
        Success
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype UsageRecordResultStatus = UsageRecordResultStatus' Lude.Text
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

pattern CustomerNotSubscribed :: UsageRecordResultStatus
pattern CustomerNotSubscribed = UsageRecordResultStatus' "CustomerNotSubscribed"

pattern DuplicateRecord :: UsageRecordResultStatus
pattern DuplicateRecord = UsageRecordResultStatus' "DuplicateRecord"

pattern Success :: UsageRecordResultStatus
pattern Success = UsageRecordResultStatus' "Success"

{-# COMPLETE
  CustomerNotSubscribed,
  DuplicateRecord,
  Success,
  UsageRecordResultStatus'
  #-}
