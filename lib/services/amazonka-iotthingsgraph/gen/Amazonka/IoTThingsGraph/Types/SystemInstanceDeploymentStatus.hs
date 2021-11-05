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
-- Module      : Amazonka.IoTThingsGraph.Types.SystemInstanceDeploymentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.SystemInstanceDeploymentStatus
  ( SystemInstanceDeploymentStatus
      ( ..,
        SystemInstanceDeploymentStatus_BOOTSTRAP,
        SystemInstanceDeploymentStatus_DELETED_IN_TARGET,
        SystemInstanceDeploymentStatus_DEPLOYED_IN_TARGET,
        SystemInstanceDeploymentStatus_DEPLOY_IN_PROGRESS,
        SystemInstanceDeploymentStatus_FAILED,
        SystemInstanceDeploymentStatus_NOT_DEPLOYED,
        SystemInstanceDeploymentStatus_PENDING_DELETE,
        SystemInstanceDeploymentStatus_UNDEPLOY_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SystemInstanceDeploymentStatus = SystemInstanceDeploymentStatus'
  { fromSystemInstanceDeploymentStatus ::
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

pattern SystemInstanceDeploymentStatus_BOOTSTRAP :: SystemInstanceDeploymentStatus
pattern SystemInstanceDeploymentStatus_BOOTSTRAP = SystemInstanceDeploymentStatus' "BOOTSTRAP"

pattern SystemInstanceDeploymentStatus_DELETED_IN_TARGET :: SystemInstanceDeploymentStatus
pattern SystemInstanceDeploymentStatus_DELETED_IN_TARGET = SystemInstanceDeploymentStatus' "DELETED_IN_TARGET"

pattern SystemInstanceDeploymentStatus_DEPLOYED_IN_TARGET :: SystemInstanceDeploymentStatus
pattern SystemInstanceDeploymentStatus_DEPLOYED_IN_TARGET = SystemInstanceDeploymentStatus' "DEPLOYED_IN_TARGET"

pattern SystemInstanceDeploymentStatus_DEPLOY_IN_PROGRESS :: SystemInstanceDeploymentStatus
pattern SystemInstanceDeploymentStatus_DEPLOY_IN_PROGRESS = SystemInstanceDeploymentStatus' "DEPLOY_IN_PROGRESS"

pattern SystemInstanceDeploymentStatus_FAILED :: SystemInstanceDeploymentStatus
pattern SystemInstanceDeploymentStatus_FAILED = SystemInstanceDeploymentStatus' "FAILED"

pattern SystemInstanceDeploymentStatus_NOT_DEPLOYED :: SystemInstanceDeploymentStatus
pattern SystemInstanceDeploymentStatus_NOT_DEPLOYED = SystemInstanceDeploymentStatus' "NOT_DEPLOYED"

pattern SystemInstanceDeploymentStatus_PENDING_DELETE :: SystemInstanceDeploymentStatus
pattern SystemInstanceDeploymentStatus_PENDING_DELETE = SystemInstanceDeploymentStatus' "PENDING_DELETE"

pattern SystemInstanceDeploymentStatus_UNDEPLOY_IN_PROGRESS :: SystemInstanceDeploymentStatus
pattern SystemInstanceDeploymentStatus_UNDEPLOY_IN_PROGRESS = SystemInstanceDeploymentStatus' "UNDEPLOY_IN_PROGRESS"

{-# COMPLETE
  SystemInstanceDeploymentStatus_BOOTSTRAP,
  SystemInstanceDeploymentStatus_DELETED_IN_TARGET,
  SystemInstanceDeploymentStatus_DEPLOYED_IN_TARGET,
  SystemInstanceDeploymentStatus_DEPLOY_IN_PROGRESS,
  SystemInstanceDeploymentStatus_FAILED,
  SystemInstanceDeploymentStatus_NOT_DEPLOYED,
  SystemInstanceDeploymentStatus_PENDING_DELETE,
  SystemInstanceDeploymentStatus_UNDEPLOY_IN_PROGRESS,
  SystemInstanceDeploymentStatus'
  #-}
