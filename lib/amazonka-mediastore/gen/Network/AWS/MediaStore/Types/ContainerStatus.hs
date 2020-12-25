{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.ContainerStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.ContainerStatus
  ( ContainerStatus
      ( ContainerStatus',
        ContainerStatusActive,
        ContainerStatusCreating,
        ContainerStatusDeleting,
        fromContainerStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ContainerStatus = ContainerStatus'
  { fromContainerStatus ::
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

pattern ContainerStatusActive :: ContainerStatus
pattern ContainerStatusActive = ContainerStatus' "ACTIVE"

pattern ContainerStatusCreating :: ContainerStatus
pattern ContainerStatusCreating = ContainerStatus' "CREATING"

pattern ContainerStatusDeleting :: ContainerStatus
pattern ContainerStatusDeleting = ContainerStatus' "DELETING"

{-# COMPLETE
  ContainerStatusActive,
  ContainerStatusCreating,
  ContainerStatusDeleting,
  ContainerStatus'
  #-}
