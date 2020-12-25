{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DomainControllerStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DomainControllerStatus
  ( DomainControllerStatus
      ( DomainControllerStatus',
        DomainControllerStatusCreating,
        DomainControllerStatusActive,
        DomainControllerStatusImpaired,
        DomainControllerStatusRestoring,
        DomainControllerStatusDeleting,
        DomainControllerStatusDeleted,
        DomainControllerStatusFailed,
        fromDomainControllerStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DomainControllerStatus = DomainControllerStatus'
  { fromDomainControllerStatus ::
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

pattern DomainControllerStatusCreating :: DomainControllerStatus
pattern DomainControllerStatusCreating = DomainControllerStatus' "Creating"

pattern DomainControllerStatusActive :: DomainControllerStatus
pattern DomainControllerStatusActive = DomainControllerStatus' "Active"

pattern DomainControllerStatusImpaired :: DomainControllerStatus
pattern DomainControllerStatusImpaired = DomainControllerStatus' "Impaired"

pattern DomainControllerStatusRestoring :: DomainControllerStatus
pattern DomainControllerStatusRestoring = DomainControllerStatus' "Restoring"

pattern DomainControllerStatusDeleting :: DomainControllerStatus
pattern DomainControllerStatusDeleting = DomainControllerStatus' "Deleting"

pattern DomainControllerStatusDeleted :: DomainControllerStatus
pattern DomainControllerStatusDeleted = DomainControllerStatus' "Deleted"

pattern DomainControllerStatusFailed :: DomainControllerStatus
pattern DomainControllerStatusFailed = DomainControllerStatus' "Failed"

{-# COMPLETE
  DomainControllerStatusCreating,
  DomainControllerStatusActive,
  DomainControllerStatusImpaired,
  DomainControllerStatusRestoring,
  DomainControllerStatusDeleting,
  DomainControllerStatusDeleted,
  DomainControllerStatusFailed,
  DomainControllerStatus'
  #-}
