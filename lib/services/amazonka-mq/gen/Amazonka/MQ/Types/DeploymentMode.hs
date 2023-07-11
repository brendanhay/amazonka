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
-- Module      : Amazonka.MQ.Types.DeploymentMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.DeploymentMode
  ( DeploymentMode
      ( ..,
        DeploymentMode_ACTIVE_STANDBY_MULTI_AZ,
        DeploymentMode_CLUSTER_MULTI_AZ,
        DeploymentMode_SINGLE_INSTANCE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The broker\'s deployment mode.
newtype DeploymentMode = DeploymentMode'
  { fromDeploymentMode ::
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

pattern DeploymentMode_ACTIVE_STANDBY_MULTI_AZ :: DeploymentMode
pattern DeploymentMode_ACTIVE_STANDBY_MULTI_AZ = DeploymentMode' "ACTIVE_STANDBY_MULTI_AZ"

pattern DeploymentMode_CLUSTER_MULTI_AZ :: DeploymentMode
pattern DeploymentMode_CLUSTER_MULTI_AZ = DeploymentMode' "CLUSTER_MULTI_AZ"

pattern DeploymentMode_SINGLE_INSTANCE :: DeploymentMode
pattern DeploymentMode_SINGLE_INSTANCE = DeploymentMode' "SINGLE_INSTANCE"

{-# COMPLETE
  DeploymentMode_ACTIVE_STANDBY_MULTI_AZ,
  DeploymentMode_CLUSTER_MULTI_AZ,
  DeploymentMode_SINGLE_INSTANCE,
  DeploymentMode'
  #-}
