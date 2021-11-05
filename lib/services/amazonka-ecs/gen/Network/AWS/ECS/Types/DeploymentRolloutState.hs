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
-- Module      : Amazonka.ECS.Types.DeploymentRolloutState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.DeploymentRolloutState
  ( DeploymentRolloutState
      ( ..,
        DeploymentRolloutState_COMPLETED,
        DeploymentRolloutState_FAILED,
        DeploymentRolloutState_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DeploymentRolloutState = DeploymentRolloutState'
  { fromDeploymentRolloutState ::
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

pattern DeploymentRolloutState_COMPLETED :: DeploymentRolloutState
pattern DeploymentRolloutState_COMPLETED = DeploymentRolloutState' "COMPLETED"

pattern DeploymentRolloutState_FAILED :: DeploymentRolloutState
pattern DeploymentRolloutState_FAILED = DeploymentRolloutState' "FAILED"

pattern DeploymentRolloutState_IN_PROGRESS :: DeploymentRolloutState
pattern DeploymentRolloutState_IN_PROGRESS = DeploymentRolloutState' "IN_PROGRESS"

{-# COMPLETE
  DeploymentRolloutState_COMPLETED,
  DeploymentRolloutState_FAILED,
  DeploymentRolloutState_IN_PROGRESS,
  DeploymentRolloutState'
  #-}
