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
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentWaitType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentWaitType
  ( DeploymentWaitType
      ( ..,
        DeploymentWaitType_READY_WAIT,
        DeploymentWaitType_TERMINATION_WAIT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DeploymentWaitType = DeploymentWaitType'
  { fromDeploymentWaitType ::
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

pattern DeploymentWaitType_READY_WAIT :: DeploymentWaitType
pattern DeploymentWaitType_READY_WAIT = DeploymentWaitType' "READY_WAIT"

pattern DeploymentWaitType_TERMINATION_WAIT :: DeploymentWaitType
pattern DeploymentWaitType_TERMINATION_WAIT = DeploymentWaitType' "TERMINATION_WAIT"

{-# COMPLETE
  DeploymentWaitType_READY_WAIT,
  DeploymentWaitType_TERMINATION_WAIT,
  DeploymentWaitType'
  #-}
