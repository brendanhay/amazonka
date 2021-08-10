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
-- Module      : Network.AWS.Route53.Types.HealthCheckType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HealthCheckType
  ( HealthCheckType
      ( ..,
        HealthCheckType_CALCULATED,
        HealthCheckType_CLOUDWATCH_METRIC,
        HealthCheckType_HTTP,
        HealthCheckType_HTTPS,
        HealthCheckType_HTTPS_STR_MATCH,
        HealthCheckType_HTTP_STR_MATCH,
        HealthCheckType_TCP
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal

newtype HealthCheckType = HealthCheckType'
  { fromHealthCheckType ::
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

pattern HealthCheckType_TCP :: HealthCheckType
pattern HealthCheckType_TCP = HealthCheckType' "TCP"

{-# COMPLETE
  HealthCheckType_CALCULATED,
  HealthCheckType_CLOUDWATCH_METRIC,
  HealthCheckType_HTTP,
  HealthCheckType_HTTPS,
  HealthCheckType_HTTPS_STR_MATCH,
  HealthCheckType_HTTP_STR_MATCH,
  HealthCheckType_TCP,
  HealthCheckType'
  #-}
