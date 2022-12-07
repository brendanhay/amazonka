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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HealthStatusFilter = HealthStatusFilter'
  { fromHealthStatusFilter ::
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
