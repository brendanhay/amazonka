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
-- Module      : Amazonka.Lightsail.Types.ContainerServiceDeploymentState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ContainerServiceDeploymentState
  ( ContainerServiceDeploymentState
      ( ..,
        ContainerServiceDeploymentState_ACTIVATING,
        ContainerServiceDeploymentState_ACTIVE,
        ContainerServiceDeploymentState_FAILED,
        ContainerServiceDeploymentState_INACTIVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ContainerServiceDeploymentState = ContainerServiceDeploymentState'
  { fromContainerServiceDeploymentState ::
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

pattern ContainerServiceDeploymentState_ACTIVATING :: ContainerServiceDeploymentState
pattern ContainerServiceDeploymentState_ACTIVATING = ContainerServiceDeploymentState' "ACTIVATING"

pattern ContainerServiceDeploymentState_ACTIVE :: ContainerServiceDeploymentState
pattern ContainerServiceDeploymentState_ACTIVE = ContainerServiceDeploymentState' "ACTIVE"

pattern ContainerServiceDeploymentState_FAILED :: ContainerServiceDeploymentState
pattern ContainerServiceDeploymentState_FAILED = ContainerServiceDeploymentState' "FAILED"

pattern ContainerServiceDeploymentState_INACTIVE :: ContainerServiceDeploymentState
pattern ContainerServiceDeploymentState_INACTIVE = ContainerServiceDeploymentState' "INACTIVE"

{-# COMPLETE
  ContainerServiceDeploymentState_ACTIVATING,
  ContainerServiceDeploymentState_ACTIVE,
  ContainerServiceDeploymentState_FAILED,
  ContainerServiceDeploymentState_INACTIVE,
  ContainerServiceDeploymentState'
  #-}
