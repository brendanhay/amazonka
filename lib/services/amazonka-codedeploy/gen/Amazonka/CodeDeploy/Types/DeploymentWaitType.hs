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
-- Module      : Amazonka.CodeDeploy.Types.DeploymentWaitType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.DeploymentWaitType
  ( DeploymentWaitType
      ( ..,
        DeploymentWaitType_READY_WAIT,
        DeploymentWaitType_TERMINATION_WAIT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeploymentWaitType = DeploymentWaitType'
  { fromDeploymentWaitType ::
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

pattern DeploymentWaitType_READY_WAIT :: DeploymentWaitType
pattern DeploymentWaitType_READY_WAIT = DeploymentWaitType' "READY_WAIT"

pattern DeploymentWaitType_TERMINATION_WAIT :: DeploymentWaitType
pattern DeploymentWaitType_TERMINATION_WAIT = DeploymentWaitType' "TERMINATION_WAIT"

{-# COMPLETE
  DeploymentWaitType_READY_WAIT,
  DeploymentWaitType_TERMINATION_WAIT,
  DeploymentWaitType'
  #-}
