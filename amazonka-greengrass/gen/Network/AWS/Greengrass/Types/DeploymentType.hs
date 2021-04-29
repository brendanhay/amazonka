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

import qualified Network.AWS.Prelude as Prelude

-- | The type of deployment. When used for \'\'CreateDeployment\'\', only
-- \'\'NewDeployment\'\' and \'\'Redeployment\'\' are valid.
newtype DeploymentType = DeploymentType'
  { fromDeploymentType ::
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
