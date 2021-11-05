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
-- Module      : Amazonka.CodeDeploy.Types.DeploymentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.DeploymentStatus
  ( DeploymentStatus
      ( ..,
        DeploymentStatus_Baking,
        DeploymentStatus_Created,
        DeploymentStatus_Failed,
        DeploymentStatus_InProgress,
        DeploymentStatus_Queued,
        DeploymentStatus_Ready,
        DeploymentStatus_Stopped,
        DeploymentStatus_Succeeded
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DeploymentStatus = DeploymentStatus'
  { fromDeploymentStatus ::
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

pattern DeploymentStatus_Baking :: DeploymentStatus
pattern DeploymentStatus_Baking = DeploymentStatus' "Baking"

pattern DeploymentStatus_Created :: DeploymentStatus
pattern DeploymentStatus_Created = DeploymentStatus' "Created"

pattern DeploymentStatus_Failed :: DeploymentStatus
pattern DeploymentStatus_Failed = DeploymentStatus' "Failed"

pattern DeploymentStatus_InProgress :: DeploymentStatus
pattern DeploymentStatus_InProgress = DeploymentStatus' "InProgress"

pattern DeploymentStatus_Queued :: DeploymentStatus
pattern DeploymentStatus_Queued = DeploymentStatus' "Queued"

pattern DeploymentStatus_Ready :: DeploymentStatus
pattern DeploymentStatus_Ready = DeploymentStatus' "Ready"

pattern DeploymentStatus_Stopped :: DeploymentStatus
pattern DeploymentStatus_Stopped = DeploymentStatus' "Stopped"

pattern DeploymentStatus_Succeeded :: DeploymentStatus
pattern DeploymentStatus_Succeeded = DeploymentStatus' "Succeeded"

{-# COMPLETE
  DeploymentStatus_Baking,
  DeploymentStatus_Created,
  DeploymentStatus_Failed,
  DeploymentStatus_InProgress,
  DeploymentStatus_Queued,
  DeploymentStatus_Ready,
  DeploymentStatus_Stopped,
  DeploymentStatus_Succeeded,
  DeploymentStatus'
  #-}
