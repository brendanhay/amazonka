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
-- Module      : Amazonka.GreengrassV2.Types.DeploymentComponentUpdatePolicyAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.DeploymentComponentUpdatePolicyAction
  ( DeploymentComponentUpdatePolicyAction
      ( ..,
        DeploymentComponentUpdatePolicyAction_NOTIFY_COMPONENTS,
        DeploymentComponentUpdatePolicyAction_SKIP_NOTIFY_COMPONENTS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeploymentComponentUpdatePolicyAction = DeploymentComponentUpdatePolicyAction'
  { fromDeploymentComponentUpdatePolicyAction ::
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

pattern DeploymentComponentUpdatePolicyAction_NOTIFY_COMPONENTS :: DeploymentComponentUpdatePolicyAction
pattern DeploymentComponentUpdatePolicyAction_NOTIFY_COMPONENTS = DeploymentComponentUpdatePolicyAction' "NOTIFY_COMPONENTS"

pattern DeploymentComponentUpdatePolicyAction_SKIP_NOTIFY_COMPONENTS :: DeploymentComponentUpdatePolicyAction
pattern DeploymentComponentUpdatePolicyAction_SKIP_NOTIFY_COMPONENTS = DeploymentComponentUpdatePolicyAction' "SKIP_NOTIFY_COMPONENTS"

{-# COMPLETE
  DeploymentComponentUpdatePolicyAction_NOTIFY_COMPONENTS,
  DeploymentComponentUpdatePolicyAction_SKIP_NOTIFY_COMPONENTS,
  DeploymentComponentUpdatePolicyAction'
  #-}
