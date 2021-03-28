{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBProxyStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.DBProxyStatus
  ( DBProxyStatus
    ( DBProxyStatus'
    , DBProxyStatusAvailable
    , DBProxyStatusModifying
    , DBProxyStatusIncompatibleNetwork
    , DBProxyStatusInsufficientResourceLimits
    , DBProxyStatusCreating
    , DBProxyStatusDeleting
    , DBProxyStatusSuspended
    , DBProxyStatusSuspending
    , DBProxyStatusReactivating
    , fromDBProxyStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DBProxyStatus = DBProxyStatus'{fromDBProxyStatus ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern DBProxyStatusAvailable :: DBProxyStatus
pattern DBProxyStatusAvailable = DBProxyStatus' "available"

pattern DBProxyStatusModifying :: DBProxyStatus
pattern DBProxyStatusModifying = DBProxyStatus' "modifying"

pattern DBProxyStatusIncompatibleNetwork :: DBProxyStatus
pattern DBProxyStatusIncompatibleNetwork = DBProxyStatus' "incompatible-network"

pattern DBProxyStatusInsufficientResourceLimits :: DBProxyStatus
pattern DBProxyStatusInsufficientResourceLimits = DBProxyStatus' "insufficient-resource-limits"

pattern DBProxyStatusCreating :: DBProxyStatus
pattern DBProxyStatusCreating = DBProxyStatus' "creating"

pattern DBProxyStatusDeleting :: DBProxyStatus
pattern DBProxyStatusDeleting = DBProxyStatus' "deleting"

pattern DBProxyStatusSuspended :: DBProxyStatus
pattern DBProxyStatusSuspended = DBProxyStatus' "suspended"

pattern DBProxyStatusSuspending :: DBProxyStatus
pattern DBProxyStatusSuspending = DBProxyStatus' "suspending"

pattern DBProxyStatusReactivating :: DBProxyStatus
pattern DBProxyStatusReactivating = DBProxyStatus' "reactivating"

{-# COMPLETE 
  DBProxyStatusAvailable,

  DBProxyStatusModifying,

  DBProxyStatusIncompatibleNetwork,

  DBProxyStatusInsufficientResourceLimits,

  DBProxyStatusCreating,

  DBProxyStatusDeleting,

  DBProxyStatusSuspended,

  DBProxyStatusSuspending,

  DBProxyStatusReactivating,
  DBProxyStatus'
  #-}
