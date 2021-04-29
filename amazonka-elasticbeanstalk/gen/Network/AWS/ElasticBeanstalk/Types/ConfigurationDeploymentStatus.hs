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

import qualified Network.AWS.Prelude as Prelude

newtype ConfigurationDeploymentStatus = ConfigurationDeploymentStatus'
  { fromConfigurationDeploymentStatus ::
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
