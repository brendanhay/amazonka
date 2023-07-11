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
-- Module      : Amazonka.CodeDeploy.Types.DeploymentCreator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.DeploymentCreator
  ( DeploymentCreator
      ( ..,
        DeploymentCreator_Autoscaling,
        DeploymentCreator_CloudFormation,
        DeploymentCreator_CloudFormationRollback,
        DeploymentCreator_CodeDeploy,
        DeploymentCreator_CodeDeployAutoUpdate,
        DeploymentCreator_CodeDeployRollback,
        DeploymentCreator_User
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeploymentCreator = DeploymentCreator'
  { fromDeploymentCreator ::
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

pattern DeploymentCreator_Autoscaling :: DeploymentCreator
pattern DeploymentCreator_Autoscaling = DeploymentCreator' "autoscaling"

pattern DeploymentCreator_CloudFormation :: DeploymentCreator
pattern DeploymentCreator_CloudFormation = DeploymentCreator' "CloudFormation"

pattern DeploymentCreator_CloudFormationRollback :: DeploymentCreator
pattern DeploymentCreator_CloudFormationRollback = DeploymentCreator' "CloudFormationRollback"

pattern DeploymentCreator_CodeDeploy :: DeploymentCreator
pattern DeploymentCreator_CodeDeploy = DeploymentCreator' "CodeDeploy"

pattern DeploymentCreator_CodeDeployAutoUpdate :: DeploymentCreator
pattern DeploymentCreator_CodeDeployAutoUpdate = DeploymentCreator' "CodeDeployAutoUpdate"

pattern DeploymentCreator_CodeDeployRollback :: DeploymentCreator
pattern DeploymentCreator_CodeDeployRollback = DeploymentCreator' "codeDeployRollback"

pattern DeploymentCreator_User :: DeploymentCreator
pattern DeploymentCreator_User = DeploymentCreator' "user"

{-# COMPLETE
  DeploymentCreator_Autoscaling,
  DeploymentCreator_CloudFormation,
  DeploymentCreator_CloudFormationRollback,
  DeploymentCreator_CodeDeploy,
  DeploymentCreator_CodeDeployAutoUpdate,
  DeploymentCreator_CodeDeployRollback,
  DeploymentCreator_User,
  DeploymentCreator'
  #-}
