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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.RouteState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.RouteState
  ( RouteState
      ( ..,
        RouteState_ACTIVE,
        RouteState_CREATING,
        RouteState_DELETING,
        RouteState_FAILED,
        RouteState_INACTIVE,
        RouteState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RouteState = RouteState'
  { fromRouteState ::
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

pattern RouteState_ACTIVE :: RouteState
pattern RouteState_ACTIVE = RouteState' "ACTIVE"

pattern RouteState_CREATING :: RouteState
pattern RouteState_CREATING = RouteState' "CREATING"

pattern RouteState_DELETING :: RouteState
pattern RouteState_DELETING = RouteState' "DELETING"

pattern RouteState_FAILED :: RouteState
pattern RouteState_FAILED = RouteState' "FAILED"

pattern RouteState_INACTIVE :: RouteState
pattern RouteState_INACTIVE = RouteState' "INACTIVE"

pattern RouteState_UPDATING :: RouteState
pattern RouteState_UPDATING = RouteState' "UPDATING"

{-# COMPLETE
  RouteState_ACTIVE,
  RouteState_CREATING,
  RouteState_DELETING,
  RouteState_FAILED,
  RouteState_INACTIVE,
  RouteState_UPDATING,
  RouteState'
  #-}
