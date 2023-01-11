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
-- Module      : Amazonka.ECS.Types.HealthStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.HealthStatus
  ( HealthStatus
      ( ..,
        HealthStatus_HEALTHY,
        HealthStatus_UNHEALTHY,
        HealthStatus_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HealthStatus = HealthStatus'
  { fromHealthStatus ::
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
