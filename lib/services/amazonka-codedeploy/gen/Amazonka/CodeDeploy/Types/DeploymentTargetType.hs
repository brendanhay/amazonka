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
-- Module      : Amazonka.CodeDeploy.Types.DeploymentTargetType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.DeploymentTargetType
  ( DeploymentTargetType
      ( ..,
        DeploymentTargetType_CloudFormationTarget,
        DeploymentTargetType_ECSTarget,
        DeploymentTargetType_InstanceTarget,
        DeploymentTargetType_LambdaTarget
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeploymentTargetType = DeploymentTargetType'
  { fromDeploymentTargetType ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern DeploymentTargetType_CloudFormationTarget :: DeploymentTargetType
pattern DeploymentTargetType_CloudFormationTarget = DeploymentTargetType' "CloudFormationTarget"

pattern DeploymentTargetType_ECSTarget :: DeploymentTargetType
pattern DeploymentTargetType_ECSTarget = DeploymentTargetType' "ECSTarget"

pattern DeploymentTargetType_InstanceTarget :: DeploymentTargetType
pattern DeploymentTargetType_InstanceTarget = DeploymentTargetType' "InstanceTarget"

pattern DeploymentTargetType_LambdaTarget :: DeploymentTargetType
pattern DeploymentTargetType_LambdaTarget = DeploymentTargetType' "LambdaTarget"

{-# COMPLETE
  DeploymentTargetType_CloudFormationTarget,
  DeploymentTargetType_ECSTarget,
  DeploymentTargetType_InstanceTarget,
  DeploymentTargetType_LambdaTarget,
  DeploymentTargetType'
  #-}
