{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.DeploymentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.DeploymentType
  ( DeploymentType
      ( DeploymentType',
        ForceResetDeployment,
        NewDeployment,
        Redeployment,
        ResetDeployment
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The type of deployment. When used for ''CreateDeployment'', only ''NewDeployment'' and ''Redeployment'' are valid.
newtype DeploymentType = DeploymentType' Lude.Text
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

pattern ForceResetDeployment :: DeploymentType
pattern ForceResetDeployment = DeploymentType' "ForceResetDeployment"

pattern NewDeployment :: DeploymentType
pattern NewDeployment = DeploymentType' "NewDeployment"

pattern Redeployment :: DeploymentType
pattern Redeployment = DeploymentType' "Redeployment"

pattern ResetDeployment :: DeploymentType
pattern ResetDeployment = DeploymentType' "ResetDeployment"

{-# COMPLETE
  ForceResetDeployment,
  NewDeployment,
  Redeployment,
  ResetDeployment,
  DeploymentType'
  #-}
