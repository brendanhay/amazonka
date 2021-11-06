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
-- Module      : Amazonka.RobOMaker.Types.DeploymentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.DeploymentStatus
  ( DeploymentStatus
      ( ..,
        DeploymentStatus_Canceled,
        DeploymentStatus_Failed,
        DeploymentStatus_InProgress,
        DeploymentStatus_Pending,
        DeploymentStatus_Preparing,
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

pattern DeploymentStatus_Canceled :: DeploymentStatus
pattern DeploymentStatus_Canceled = DeploymentStatus' "Canceled"

pattern DeploymentStatus_Failed :: DeploymentStatus
pattern DeploymentStatus_Failed = DeploymentStatus' "Failed"

pattern DeploymentStatus_InProgress :: DeploymentStatus
pattern DeploymentStatus_InProgress = DeploymentStatus' "InProgress"

pattern DeploymentStatus_Pending :: DeploymentStatus
pattern DeploymentStatus_Pending = DeploymentStatus' "Pending"

pattern DeploymentStatus_Preparing :: DeploymentStatus
pattern DeploymentStatus_Preparing = DeploymentStatus' "Preparing"

pattern DeploymentStatus_Succeeded :: DeploymentStatus
pattern DeploymentStatus_Succeeded = DeploymentStatus' "Succeeded"

{-# COMPLETE
  DeploymentStatus_Canceled,
  DeploymentStatus_Failed,
  DeploymentStatus_InProgress,
  DeploymentStatus_Pending,
  DeploymentStatus_Preparing,
  DeploymentStatus_Succeeded,
  DeploymentStatus'
  #-}
