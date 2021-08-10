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
-- Module      : Network.AWS.ECS.Types.DeploymentControllerType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DeploymentControllerType
  ( DeploymentControllerType
      ( ..,
        DeploymentControllerType_CODE_DEPLOY,
        DeploymentControllerType_ECS,
        DeploymentControllerType_EXTERNAL
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DeploymentControllerType = DeploymentControllerType'
  { fromDeploymentControllerType ::
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

pattern DeploymentControllerType_CODE_DEPLOY :: DeploymentControllerType
pattern DeploymentControllerType_CODE_DEPLOY = DeploymentControllerType' "CODE_DEPLOY"

pattern DeploymentControllerType_ECS :: DeploymentControllerType
pattern DeploymentControllerType_ECS = DeploymentControllerType' "ECS"

pattern DeploymentControllerType_EXTERNAL :: DeploymentControllerType
pattern DeploymentControllerType_EXTERNAL = DeploymentControllerType' "EXTERNAL"

{-# COMPLETE
  DeploymentControllerType_CODE_DEPLOY,
  DeploymentControllerType_ECS,
  DeploymentControllerType_EXTERNAL,
  DeploymentControllerType'
  #-}
