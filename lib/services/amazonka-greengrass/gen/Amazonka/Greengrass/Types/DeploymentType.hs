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
-- Module      : Amazonka.Greengrass.Types.DeploymentType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.DeploymentType
  ( DeploymentType
      ( ..,
        DeploymentType_ForceResetDeployment,
        DeploymentType_NewDeployment,
        DeploymentType_Redeployment,
        DeploymentType_ResetDeployment
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The type of deployment. When used for \'\'CreateDeployment\'\', only
-- \'\'NewDeployment\'\' and \'\'Redeployment\'\' are valid.
newtype DeploymentType = DeploymentType'
  { fromDeploymentType ::
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
