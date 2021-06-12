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
-- Module      : Network.AWS.ElasticBeanstalk.Types.ConfigurationDeploymentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ConfigurationDeploymentStatus
  ( ConfigurationDeploymentStatus
      ( ..,
        ConfigurationDeploymentStatus_Deployed,
        ConfigurationDeploymentStatus_Failed,
        ConfigurationDeploymentStatus_Pending
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ConfigurationDeploymentStatus = ConfigurationDeploymentStatus'
  { fromConfigurationDeploymentStatus ::
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
