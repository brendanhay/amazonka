{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DomainStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.DomainStatus
  ( DomainStatus
    ( DomainStatus'
    , DomainStatusDeleting
    , DomainStatusFailed
    , DomainStatusInService
    , DomainStatusPending
    , DomainStatusUpdating
    , DomainStatusUpdateFailed
    , DomainStatusDeleteFailed
    , fromDomainStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DomainStatus = DomainStatus'{fromDomainStatus :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern DomainStatusDeleting :: DomainStatus
pattern DomainStatusDeleting = DomainStatus' "Deleting"

pattern DomainStatusFailed :: DomainStatus
pattern DomainStatusFailed = DomainStatus' "Failed"

pattern DomainStatusInService :: DomainStatus
pattern DomainStatusInService = DomainStatus' "InService"

pattern DomainStatusPending :: DomainStatus
pattern DomainStatusPending = DomainStatus' "Pending"

pattern DomainStatusUpdating :: DomainStatus
pattern DomainStatusUpdating = DomainStatus' "Updating"

pattern DomainStatusUpdateFailed :: DomainStatus
pattern DomainStatusUpdateFailed = DomainStatus' "Update_Failed"

pattern DomainStatusDeleteFailed :: DomainStatus
pattern DomainStatusDeleteFailed = DomainStatus' "Delete_Failed"

{-# COMPLETE 
  DomainStatusDeleting,

  DomainStatusFailed,

  DomainStatusInService,

  DomainStatusPending,

  DomainStatusUpdating,

  DomainStatusUpdateFailed,

  DomainStatusDeleteFailed,
  DomainStatus'
  #-}
