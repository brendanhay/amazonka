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
-- Module      : Network.AWS.MarketplaceMetering.Types.UsageRecordResultStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceMetering.Types.UsageRecordResultStatus
  ( UsageRecordResultStatus
      ( ..,
        UsageRecordResultStatus_CustomerNotSubscribed,
        UsageRecordResultStatus_DuplicateRecord,
        UsageRecordResultStatus_Success
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype UsageRecordResultStatus = UsageRecordResultStatus'
  { fromUsageRecordResultStatus ::
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

pattern UsageRecordResultStatus_CustomerNotSubscribed :: UsageRecordResultStatus
pattern UsageRecordResultStatus_CustomerNotSubscribed = UsageRecordResultStatus' "CustomerNotSubscribed"

pattern UsageRecordResultStatus_DuplicateRecord :: UsageRecordResultStatus
pattern UsageRecordResultStatus_DuplicateRecord = UsageRecordResultStatus' "DuplicateRecord"

pattern UsageRecordResultStatus_Success :: UsageRecordResultStatus
pattern UsageRecordResultStatus_Success = UsageRecordResultStatus' "Success"

{-# COMPLETE
  UsageRecordResultStatus_CustomerNotSubscribed,
  UsageRecordResultStatus_DuplicateRecord,
  UsageRecordResultStatus_Success,
  UsageRecordResultStatus'
  #-}
