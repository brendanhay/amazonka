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
-- Module      : Amazonka.M2.Types.DeploymentLifecycle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.DeploymentLifecycle
  ( DeploymentLifecycle
      ( ..,
        DeploymentLifecycle_Deploying,
        DeploymentLifecycle_Failed,
        DeploymentLifecycle_Succeeded
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DeploymentLifecycle = DeploymentLifecycle'
  { fromDeploymentLifecycle ::
      Core.Text
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

pattern DeploymentLifecycle_Deploying :: DeploymentLifecycle
pattern DeploymentLifecycle_Deploying = DeploymentLifecycle' "Deploying"

pattern DeploymentLifecycle_Failed :: DeploymentLifecycle
pattern DeploymentLifecycle_Failed = DeploymentLifecycle' "Failed"

pattern DeploymentLifecycle_Succeeded :: DeploymentLifecycle
pattern DeploymentLifecycle_Succeeded = DeploymentLifecycle' "Succeeded"

{-# COMPLETE
  DeploymentLifecycle_Deploying,
  DeploymentLifecycle_Failed,
  DeploymentLifecycle_Succeeded,
  DeploymentLifecycle'
  #-}
