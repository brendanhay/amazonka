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
-- Module      : Amazonka.Route53AutoNaming.Types.HealthStatusFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.HealthStatusFilter
  ( HealthStatusFilter
      ( ..,
        HealthStatusFilter_ALL,
        HealthStatusFilter_HEALTHY,
        HealthStatusFilter_HEALTHY_OR_ELSE_ALL,
        HealthStatusFilter_UNHEALTHY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype HealthStatusFilter = HealthStatusFilter'
  { fromHealthStatusFilter ::
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

pattern HealthStatusFilter_ALL :: HealthStatusFilter
pattern HealthStatusFilter_ALL = HealthStatusFilter' "ALL"

pattern HealthStatusFilter_HEALTHY :: HealthStatusFilter
pattern HealthStatusFilter_HEALTHY = HealthStatusFilter' "HEALTHY"

pattern HealthStatusFilter_HEALTHY_OR_ELSE_ALL :: HealthStatusFilter
pattern HealthStatusFilter_HEALTHY_OR_ELSE_ALL = HealthStatusFilter' "HEALTHY_OR_ELSE_ALL"

pattern HealthStatusFilter_UNHEALTHY :: HealthStatusFilter
pattern HealthStatusFilter_UNHEALTHY = HealthStatusFilter' "UNHEALTHY"

{-# COMPLETE
  HealthStatusFilter_ALL,
  HealthStatusFilter_HEALTHY,
  HealthStatusFilter_HEALTHY_OR_ELSE_ALL,
  HealthStatusFilter_UNHEALTHY,
  HealthStatusFilter'
  #-}
