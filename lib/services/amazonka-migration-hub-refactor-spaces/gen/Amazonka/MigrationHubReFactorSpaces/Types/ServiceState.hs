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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.ServiceState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.ServiceState
  ( ServiceState
      ( ..,
        ServiceState_ACTIVE,
        ServiceState_CREATING,
        ServiceState_DELETING,
        ServiceState_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ServiceState = ServiceState'
  { fromServiceState ::
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

pattern ServiceState_ACTIVE :: ServiceState
pattern ServiceState_ACTIVE = ServiceState' "ACTIVE"

pattern ServiceState_CREATING :: ServiceState
pattern ServiceState_CREATING = ServiceState' "CREATING"

pattern ServiceState_DELETING :: ServiceState
pattern ServiceState_DELETING = ServiceState' "DELETING"

pattern ServiceState_FAILED :: ServiceState
pattern ServiceState_FAILED = ServiceState' "FAILED"

{-# COMPLETE
  ServiceState_ACTIVE,
  ServiceState_CREATING,
  ServiceState_DELETING,
  ServiceState_FAILED,
  ServiceState'
  #-}
