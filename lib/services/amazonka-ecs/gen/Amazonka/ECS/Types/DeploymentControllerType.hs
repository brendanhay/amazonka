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
-- Module      : Amazonka.ECS.Types.DeploymentControllerType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.DeploymentControllerType
  ( DeploymentControllerType
      ( ..,
        DeploymentControllerType_CODE_DEPLOY,
        DeploymentControllerType_ECS,
        DeploymentControllerType_EXTERNAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeploymentControllerType = DeploymentControllerType'
  { fromDeploymentControllerType ::
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
