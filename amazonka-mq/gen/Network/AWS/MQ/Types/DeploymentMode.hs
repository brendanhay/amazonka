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
-- Module      : Network.AWS.MQ.Types.DeploymentMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.DeploymentMode
  ( DeploymentMode
      ( ..,
        DeploymentMode_ACTIVE_STANDBY_MULTI_AZ,
        DeploymentMode_CLUSTER_MULTI_AZ,
        DeploymentMode_SINGLE_INSTANCE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | The deployment mode of the broker.
newtype DeploymentMode = DeploymentMode'
  { fromDeploymentMode ::
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
