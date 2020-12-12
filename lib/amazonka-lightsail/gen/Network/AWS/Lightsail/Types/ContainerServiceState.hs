{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceState
  ( ContainerServiceState
      ( ContainerServiceState',
        Deleting,
        Disabled,
        Pending,
        Ready,
        Running,
        Updating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ContainerServiceState = ContainerServiceState' Lude.Text
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

pattern Deleting :: ContainerServiceState
pattern Deleting = ContainerServiceState' "DELETING"

pattern Disabled :: ContainerServiceState
pattern Disabled = ContainerServiceState' "DISABLED"

pattern Pending :: ContainerServiceState
pattern Pending = ContainerServiceState' "PENDING"

pattern Ready :: ContainerServiceState
pattern Ready = ContainerServiceState' "READY"

pattern Running :: ContainerServiceState
pattern Running = ContainerServiceState' "RUNNING"

pattern Updating :: ContainerServiceState
pattern Updating = ContainerServiceState' "UPDATING"

{-# COMPLETE
  Deleting,
  Disabled,
  Pending,
  Ready,
  Running,
  Updating,
  ContainerServiceState'
  #-}
