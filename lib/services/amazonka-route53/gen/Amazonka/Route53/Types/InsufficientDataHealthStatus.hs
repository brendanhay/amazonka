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
-- Module      : Amazonka.Route53.Types.InsufficientDataHealthStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.InsufficientDataHealthStatus
  ( InsufficientDataHealthStatus
      ( ..,
        InsufficientDataHealthStatus_Healthy,
        InsufficientDataHealthStatus_LastKnownStatus,
        InsufficientDataHealthStatus_Unhealthy
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

newtype InsufficientDataHealthStatus = InsufficientDataHealthStatus'
  { fromInsufficientDataHealthStatus ::
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

pattern InsufficientDataHealthStatus_Healthy :: InsufficientDataHealthStatus
pattern InsufficientDataHealthStatus_Healthy = InsufficientDataHealthStatus' "Healthy"

pattern InsufficientDataHealthStatus_LastKnownStatus :: InsufficientDataHealthStatus
pattern InsufficientDataHealthStatus_LastKnownStatus = InsufficientDataHealthStatus' "LastKnownStatus"

pattern InsufficientDataHealthStatus_Unhealthy :: InsufficientDataHealthStatus
pattern InsufficientDataHealthStatus_Unhealthy = InsufficientDataHealthStatus' "Unhealthy"

{-# COMPLETE
  InsufficientDataHealthStatus_Healthy,
  InsufficientDataHealthStatus_LastKnownStatus,
  InsufficientDataHealthStatus_Unhealthy,
  InsufficientDataHealthStatus'
  #-}
