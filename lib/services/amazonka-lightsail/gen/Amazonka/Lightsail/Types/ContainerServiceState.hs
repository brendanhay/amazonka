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
-- Module      : Amazonka.Lightsail.Types.ContainerServiceState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ContainerServiceState
  ( ContainerServiceState
      ( ..,
        ContainerServiceState_DELETING,
        ContainerServiceState_DEPLOYING,
        ContainerServiceState_DISABLED,
        ContainerServiceState_PENDING,
        ContainerServiceState_READY,
        ContainerServiceState_RUNNING,
        ContainerServiceState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ContainerServiceState = ContainerServiceState'
  { fromContainerServiceState ::
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

pattern ContainerServiceState_DELETING :: ContainerServiceState
pattern ContainerServiceState_DELETING = ContainerServiceState' "DELETING"

pattern ContainerServiceState_DEPLOYING :: ContainerServiceState
pattern ContainerServiceState_DEPLOYING = ContainerServiceState' "DEPLOYING"

pattern ContainerServiceState_DISABLED :: ContainerServiceState
pattern ContainerServiceState_DISABLED = ContainerServiceState' "DISABLED"

pattern ContainerServiceState_PENDING :: ContainerServiceState
pattern ContainerServiceState_PENDING = ContainerServiceState' "PENDING"

pattern ContainerServiceState_READY :: ContainerServiceState
pattern ContainerServiceState_READY = ContainerServiceState' "READY"

pattern ContainerServiceState_RUNNING :: ContainerServiceState
pattern ContainerServiceState_RUNNING = ContainerServiceState' "RUNNING"

pattern ContainerServiceState_UPDATING :: ContainerServiceState
pattern ContainerServiceState_UPDATING = ContainerServiceState' "UPDATING"

{-# COMPLETE
  ContainerServiceState_DELETING,
  ContainerServiceState_DEPLOYING,
  ContainerServiceState_DISABLED,
  ContainerServiceState_PENDING,
  ContainerServiceState_READY,
  ContainerServiceState_RUNNING,
  ContainerServiceState_UPDATING,
  ContainerServiceState'
  #-}
