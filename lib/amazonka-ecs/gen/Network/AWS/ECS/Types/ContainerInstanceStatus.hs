{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerInstanceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerInstanceStatus
  ( ContainerInstanceStatus
      ( ContainerInstanceStatus',
        Active,
        Deregistering,
        Draining,
        Registering,
        RegistrationFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ContainerInstanceStatus = ContainerInstanceStatus' Lude.Text
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

pattern Active :: ContainerInstanceStatus
pattern Active = ContainerInstanceStatus' "ACTIVE"

pattern Deregistering :: ContainerInstanceStatus
pattern Deregistering = ContainerInstanceStatus' "DEREGISTERING"

pattern Draining :: ContainerInstanceStatus
pattern Draining = ContainerInstanceStatus' "DRAINING"

pattern Registering :: ContainerInstanceStatus
pattern Registering = ContainerInstanceStatus' "REGISTERING"

pattern RegistrationFailed :: ContainerInstanceStatus
pattern RegistrationFailed = ContainerInstanceStatus' "REGISTRATION_FAILED"

{-# COMPLETE
  Active,
  Deregistering,
  Draining,
  Registering,
  RegistrationFailed,
  ContainerInstanceStatus'
  #-}
