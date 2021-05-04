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
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentTargetType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentTargetType
  ( DeploymentTargetType
      ( ..,
        DeploymentTargetType_CloudFormationTarget,
        DeploymentTargetType_ECSTarget,
        DeploymentTargetType_InstanceTarget,
        DeploymentTargetType_LambdaTarget
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DeploymentTargetType = DeploymentTargetType'
  { fromDeploymentTargetType ::
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
