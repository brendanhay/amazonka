-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceDeploymentState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceDeploymentState
  ( ContainerServiceDeploymentState
      ( ContainerServiceDeploymentState',
        Activating,
        Active,
        Failed,
        Inactive
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ContainerServiceDeploymentState = ContainerServiceDeploymentState' Lude.Text
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

pattern Activating :: ContainerServiceDeploymentState
pattern Activating = ContainerServiceDeploymentState' "ACTIVATING"

pattern Active :: ContainerServiceDeploymentState
pattern Active = ContainerServiceDeploymentState' "ACTIVE"

pattern Failed :: ContainerServiceDeploymentState
pattern Failed = ContainerServiceDeploymentState' "FAILED"

pattern Inactive :: ContainerServiceDeploymentState
pattern Inactive = ContainerServiceDeploymentState' "INACTIVE"

{-# COMPLETE
  Activating,
  Active,
  Failed,
  Inactive,
  ContainerServiceDeploymentState'
  #-}
