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
        ContainerInstanceStatusActive,
        ContainerInstanceStatusDraining,
        ContainerInstanceStatusRegistering,
        ContainerInstanceStatusDeregistering,
        ContainerInstanceStatusRegistrationFailed,
        fromContainerInstanceStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ContainerInstanceStatus = ContainerInstanceStatus'
  { fromContainerInstanceStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ContainerInstanceStatusActive :: ContainerInstanceStatus
pattern ContainerInstanceStatusActive = ContainerInstanceStatus' "ACTIVE"

pattern ContainerInstanceStatusDraining :: ContainerInstanceStatus
pattern ContainerInstanceStatusDraining = ContainerInstanceStatus' "DRAINING"

pattern ContainerInstanceStatusRegistering :: ContainerInstanceStatus
pattern ContainerInstanceStatusRegistering = ContainerInstanceStatus' "REGISTERING"

pattern ContainerInstanceStatusDeregistering :: ContainerInstanceStatus
pattern ContainerInstanceStatusDeregistering = ContainerInstanceStatus' "DEREGISTERING"

pattern ContainerInstanceStatusRegistrationFailed :: ContainerInstanceStatus
pattern ContainerInstanceStatusRegistrationFailed = ContainerInstanceStatus' "REGISTRATION_FAILED"

{-# COMPLETE
  ContainerInstanceStatusActive,
  ContainerInstanceStatusDraining,
  ContainerInstanceStatusRegistering,
  ContainerInstanceStatusDeregistering,
  ContainerInstanceStatusRegistrationFailed,
  ContainerInstanceStatus'
  #-}
