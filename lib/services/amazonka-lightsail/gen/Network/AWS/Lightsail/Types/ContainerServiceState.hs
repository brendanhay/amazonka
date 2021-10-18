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
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceState
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ContainerServiceState = ContainerServiceState'
  { fromContainerServiceState ::
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
