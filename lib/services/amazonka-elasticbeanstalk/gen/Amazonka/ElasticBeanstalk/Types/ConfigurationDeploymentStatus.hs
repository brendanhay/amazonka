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
-- Module      : Amazonka.ElasticBeanstalk.Types.ConfigurationDeploymentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.ConfigurationDeploymentStatus
  ( ConfigurationDeploymentStatus
      ( ..,
        ConfigurationDeploymentStatus_Deployed,
        ConfigurationDeploymentStatus_Failed,
        ConfigurationDeploymentStatus_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConfigurationDeploymentStatus = ConfigurationDeploymentStatus'
  { fromConfigurationDeploymentStatus ::
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

pattern ConfigurationDeploymentStatus_Deployed :: ConfigurationDeploymentStatus
pattern ConfigurationDeploymentStatus_Deployed = ConfigurationDeploymentStatus' "deployed"

pattern ConfigurationDeploymentStatus_Failed :: ConfigurationDeploymentStatus
pattern ConfigurationDeploymentStatus_Failed = ConfigurationDeploymentStatus' "failed"

pattern ConfigurationDeploymentStatus_Pending :: ConfigurationDeploymentStatus
pattern ConfigurationDeploymentStatus_Pending = ConfigurationDeploymentStatus' "pending"

{-# COMPLETE
  ConfigurationDeploymentStatus_Deployed,
  ConfigurationDeploymentStatus_Failed,
  ConfigurationDeploymentStatus_Pending,
  ConfigurationDeploymentStatus'
  #-}
