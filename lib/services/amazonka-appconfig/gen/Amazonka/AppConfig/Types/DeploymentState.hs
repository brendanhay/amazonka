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
-- Module      : Amazonka.AppConfig.Types.DeploymentState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.DeploymentState
  ( DeploymentState
      ( ..,
        DeploymentState_BAKING,
        DeploymentState_COMPLETE,
        DeploymentState_DEPLOYING,
        DeploymentState_ROLLED_BACK,
        DeploymentState_ROLLING_BACK,
        DeploymentState_VALIDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeploymentState = DeploymentState'
  { fromDeploymentState ::
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

pattern DeploymentState_BAKING :: DeploymentState
pattern DeploymentState_BAKING = DeploymentState' "BAKING"

pattern DeploymentState_COMPLETE :: DeploymentState
pattern DeploymentState_COMPLETE = DeploymentState' "COMPLETE"

pattern DeploymentState_DEPLOYING :: DeploymentState
pattern DeploymentState_DEPLOYING = DeploymentState' "DEPLOYING"

pattern DeploymentState_ROLLED_BACK :: DeploymentState
pattern DeploymentState_ROLLED_BACK = DeploymentState' "ROLLED_BACK"

pattern DeploymentState_ROLLING_BACK :: DeploymentState
pattern DeploymentState_ROLLING_BACK = DeploymentState' "ROLLING_BACK"

pattern DeploymentState_VALIDATING :: DeploymentState
pattern DeploymentState_VALIDATING = DeploymentState' "VALIDATING"

{-# COMPLETE
  DeploymentState_BAKING,
  DeploymentState_COMPLETE,
  DeploymentState_DEPLOYING,
  DeploymentState_ROLLED_BACK,
  DeploymentState_ROLLING_BACK,
  DeploymentState_VALIDATING,
  DeploymentState'
  #-}
