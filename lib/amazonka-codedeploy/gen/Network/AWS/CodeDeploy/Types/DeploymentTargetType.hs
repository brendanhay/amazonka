{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentTargetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentTargetType
  ( DeploymentTargetType
      ( DeploymentTargetType',
        CloudFormationTarget,
        ECSTarget,
        InstanceTarget,
        LambdaTarget
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DeploymentTargetType = DeploymentTargetType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CloudFormationTarget :: DeploymentTargetType
pattern CloudFormationTarget = DeploymentTargetType' "CloudFormationTarget"

pattern ECSTarget :: DeploymentTargetType
pattern ECSTarget = DeploymentTargetType' "ECSTarget"

pattern InstanceTarget :: DeploymentTargetType
pattern InstanceTarget = DeploymentTargetType' "InstanceTarget"

pattern LambdaTarget :: DeploymentTargetType
pattern LambdaTarget = DeploymentTargetType' "LambdaTarget"

{-# COMPLETE
  CloudFormationTarget,
  ECSTarget,
  InstanceTarget,
  LambdaTarget,
  DeploymentTargetType'
  #-}
