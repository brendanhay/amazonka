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
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentCreator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentCreator
  ( DeploymentCreator
      ( ..,
        DeploymentCreator_Autoscaling,
        DeploymentCreator_CloudFormation,
        DeploymentCreator_CloudFormationRollback,
        DeploymentCreator_CodeDeploy,
        DeploymentCreator_CodeDeployRollback,
        DeploymentCreator_User
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DeploymentCreator = DeploymentCreator'
  { fromDeploymentCreator ::
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

pattern DeploymentCreator_Autoscaling :: DeploymentCreator
pattern DeploymentCreator_Autoscaling = DeploymentCreator' "autoscaling"

pattern DeploymentCreator_CloudFormation :: DeploymentCreator
pattern DeploymentCreator_CloudFormation = DeploymentCreator' "CloudFormation"

pattern DeploymentCreator_CloudFormationRollback :: DeploymentCreator
pattern DeploymentCreator_CloudFormationRollback = DeploymentCreator' "CloudFormationRollback"

pattern DeploymentCreator_CodeDeploy :: DeploymentCreator
pattern DeploymentCreator_CodeDeploy = DeploymentCreator' "CodeDeploy"

pattern DeploymentCreator_CodeDeployRollback :: DeploymentCreator
pattern DeploymentCreator_CodeDeployRollback = DeploymentCreator' "codeDeployRollback"

pattern DeploymentCreator_User :: DeploymentCreator
pattern DeploymentCreator_User = DeploymentCreator' "user"

{-# COMPLETE
  DeploymentCreator_Autoscaling,
  DeploymentCreator_CloudFormation,
  DeploymentCreator_CloudFormationRollback,
  DeploymentCreator_CodeDeploy,
  DeploymentCreator_CodeDeployRollback,
  DeploymentCreator_User,
  DeploymentCreator'
  #-}
