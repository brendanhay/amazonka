-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.DeploymentMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.DeploymentMode
  ( DeploymentMode
      ( DeploymentMode',
        ActiveStandbyMultiAz,
        ClusterMultiAz,
        SingleInstance
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The deployment mode of the broker.
newtype DeploymentMode = DeploymentMode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ActiveStandbyMultiAz :: DeploymentMode
pattern ActiveStandbyMultiAz = DeploymentMode' "ACTIVE_STANDBY_MULTI_AZ"

pattern ClusterMultiAz :: DeploymentMode
pattern ClusterMultiAz = DeploymentMode' "CLUSTER_MULTI_AZ"

pattern SingleInstance :: DeploymentMode
pattern SingleInstance = DeploymentMode' "SINGLE_INSTANCE"

{-# COMPLETE
  ActiveStandbyMultiAz,
  ClusterMultiAz,
  SingleInstance,
  DeploymentMode'
  #-}
