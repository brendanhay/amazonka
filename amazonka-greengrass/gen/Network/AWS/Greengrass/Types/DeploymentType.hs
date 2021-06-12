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
-- Module      : Network.AWS.Greengrass.Types.DeploymentType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.DeploymentType
  ( DeploymentType
      ( ..,
        DeploymentType_ForceResetDeployment,
        DeploymentType_NewDeployment,
        DeploymentType_Redeployment,
        DeploymentType_ResetDeployment
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The type of deployment. When used for \'\'CreateDeployment\'\', only
-- \'\'NewDeployment\'\' and \'\'Redeployment\'\' are valid.
newtype DeploymentType = DeploymentType'
  { fromDeploymentType ::
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

pattern DeploymentType_ForceResetDeployment :: DeploymentType
pattern DeploymentType_ForceResetDeployment = DeploymentType' "ForceResetDeployment"

pattern DeploymentType_NewDeployment :: DeploymentType
pattern DeploymentType_NewDeployment = DeploymentType' "NewDeployment"

pattern DeploymentType_Redeployment :: DeploymentType
pattern DeploymentType_Redeployment = DeploymentType' "Redeployment"

pattern DeploymentType_ResetDeployment :: DeploymentType
pattern DeploymentType_ResetDeployment = DeploymentType' "ResetDeployment"

{-# COMPLETE
  DeploymentType_ForceResetDeployment,
  DeploymentType_NewDeployment,
  DeploymentType_Redeployment,
  DeploymentType_ResetDeployment,
  DeploymentType'
  #-}
