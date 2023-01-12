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
-- Module      : Amazonka.MarketplaceMetering.Types.UsageRecordResultStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceMetering.Types.UsageRecordResultStatus
  ( UsageRecordResultStatus
      ( ..,
        UsageRecordResultStatus_CustomerNotSubscribed,
        UsageRecordResultStatus_DuplicateRecord,
        UsageRecordResultStatus_Success
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UsageRecordResultStatus = UsageRecordResultStatus'
  { fromUsageRecordResultStatus ::
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
