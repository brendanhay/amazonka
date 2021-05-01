{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype DeploymentStatus = DeploymentStatus'
  { fromDeploymentStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
