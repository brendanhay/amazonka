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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

newtype RouteState = RouteState'
  { fromRouteState ::
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
