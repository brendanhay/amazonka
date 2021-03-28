{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.IpSetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.IpSetStatus
  ( IpSetStatus
    ( IpSetStatus'
    , IpSetStatusInactive
    , IpSetStatusActivating
    , IpSetStatusActive
    , IpSetStatusDeactivating
    , IpSetStatusError
    , IpSetStatusDeletePending
    , IpSetStatusDeleted
    , fromIpSetStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype IpSetStatus = IpSetStatus'{fromIpSetStatus :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern IpSetStatusInactive :: IpSetStatus
pattern IpSetStatusInactive = IpSetStatus' "INACTIVE"

pattern IpSetStatusActivating :: IpSetStatus
pattern IpSetStatusActivating = IpSetStatus' "ACTIVATING"

pattern IpSetStatusActive :: IpSetStatus
pattern IpSetStatusActive = IpSetStatus' "ACTIVE"

pattern IpSetStatusDeactivating :: IpSetStatus
pattern IpSetStatusDeactivating = IpSetStatus' "DEACTIVATING"

pattern IpSetStatusError :: IpSetStatus
pattern IpSetStatusError = IpSetStatus' "ERROR"

pattern IpSetStatusDeletePending :: IpSetStatus
pattern IpSetStatusDeletePending = IpSetStatus' "DELETE_PENDING"

pattern IpSetStatusDeleted :: IpSetStatus
pattern IpSetStatusDeleted = IpSetStatus' "DELETED"

{-# COMPLETE 
  IpSetStatusInactive,

  IpSetStatusActivating,

  IpSetStatusActive,

  IpSetStatusDeactivating,

  IpSetStatusError,

  IpSetStatusDeletePending,

  IpSetStatusDeleted,
  IpSetStatus'
  #-}
