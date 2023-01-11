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
-- Module      : Amazonka.Route53.Types.HealthCheckType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.HealthCheckType
  ( HealthCheckType
      ( ..,
        HealthCheckType_CALCULATED,
        HealthCheckType_CLOUDWATCH_METRIC,
        HealthCheckType_HTTP,
        HealthCheckType_HTTPS,
        HealthCheckType_HTTPS_STR_MATCH,
        HealthCheckType_HTTP_STR_MATCH,
        HealthCheckType_RECOVERY_CONTROL,
        HealthCheckType_TCP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

newtype HealthCheckType = HealthCheckType'
  { fromHealthCheckType ::
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

pattern HealthCheckType_CALCULATED :: HealthCheckType
pattern HealthCheckType_CALCULATED = HealthCheckType' "CALCULATED"

pattern HealthCheckType_CLOUDWATCH_METRIC :: HealthCheckType
pattern HealthCheckType_CLOUDWATCH_METRIC = HealthCheckType' "CLOUDWATCH_METRIC"

pattern HealthCheckType_HTTP :: HealthCheckType
pattern HealthCheckType_HTTP = HealthCheckType' "HTTP"

pattern HealthCheckType_HTTPS :: HealthCheckType
pattern HealthCheckType_HTTPS = HealthCheckType' "HTTPS"

pattern HealthCheckType_HTTPS_STR_MATCH :: HealthCheckType
pattern HealthCheckType_HTTPS_STR_MATCH = HealthCheckType' "HTTPS_STR_MATCH"

pattern HealthCheckType_HTTP_STR_MATCH :: HealthCheckType
pattern HealthCheckType_HTTP_STR_MATCH = HealthCheckType' "HTTP_STR_MATCH"

pattern HealthCheckType_RECOVERY_CONTROL :: HealthCheckType
pattern HealthCheckType_RECOVERY_CONTROL = HealthCheckType' "RECOVERY_CONTROL"

pattern HealthCheckType_TCP :: HealthCheckType
pattern HealthCheckType_TCP = HealthCheckType' "TCP"

{-# COMPLETE
  HealthCheckType_CALCULATED,
  HealthCheckType_CLOUDWATCH_METRIC,
  HealthCheckType_HTTP,
  HealthCheckType_HTTPS,
  HealthCheckType_HTTPS_STR_MATCH,
  HealthCheckType_HTTP_STR_MATCH,
  HealthCheckType_RECOVERY_CONTROL,
  HealthCheckType_TCP,
  HealthCheckType'
  #-}
