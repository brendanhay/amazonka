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
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentStatus
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

import qualified Network.AWS.Core as Core

newtype DeploymentStatus = DeploymentStatus'
  { fromDeploymentStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
