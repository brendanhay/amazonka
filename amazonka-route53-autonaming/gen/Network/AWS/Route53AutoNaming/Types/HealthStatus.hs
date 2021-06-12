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
-- Module      : Network.AWS.Route53AutoNaming.Types.HealthStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.HealthStatus
  ( HealthStatus
      ( ..,
        HealthStatus_HEALTHY,
        HealthStatus_UNHEALTHY,
        HealthStatus_UNKNOWN
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype HealthStatus = HealthStatus'
  { fromHealthStatus ::
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

pattern HealthStatus_HEALTHY :: HealthStatus
pattern HealthStatus_HEALTHY = HealthStatus' "HEALTHY"

pattern HealthStatus_UNHEALTHY :: HealthStatus
pattern HealthStatus_UNHEALTHY = HealthStatus' "UNHEALTHY"

pattern HealthStatus_UNKNOWN :: HealthStatus
pattern HealthStatus_UNKNOWN = HealthStatus' "UNKNOWN"

{-# COMPLETE
  HealthStatus_HEALTHY,
  HealthStatus_UNHEALTHY,
  HealthStatus_UNKNOWN,
  HealthStatus'
  #-}
