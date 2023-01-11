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
-- Module      : Amazonka.RedshiftServerLess.Types.UsageLimitUsageType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftServerLess.Types.UsageLimitUsageType
  ( UsageLimitUsageType
      ( ..,
        UsageLimitUsageType_Cross_region_datasharing,
        UsageLimitUsageType_Serverless_compute
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UsageLimitUsageType = UsageLimitUsageType'
  { fromUsageLimitUsageType ::
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

pattern UsageLimitUsageType_Cross_region_datasharing :: UsageLimitUsageType
pattern UsageLimitUsageType_Cross_region_datasharing = UsageLimitUsageType' "cross-region-datasharing"

pattern UsageLimitUsageType_Serverless_compute :: UsageLimitUsageType
pattern UsageLimitUsageType_Serverless_compute = UsageLimitUsageType' "serverless-compute"

{-# COMPLETE
  UsageLimitUsageType_Cross_region_datasharing,
  UsageLimitUsageType_Serverless_compute,
  UsageLimitUsageType'
  #-}
