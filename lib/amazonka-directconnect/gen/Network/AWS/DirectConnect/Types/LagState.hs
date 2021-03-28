{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.LagState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.LagState
  ( LagState
    ( LagState'
    , LagStateRequested
    , LagStatePending
    , LagStateAvailable
    , LagStateDown
    , LagStateDeleting
    , LagStateDeleted
    , LagStateUnknown
    , fromLagState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype LagState = LagState'{fromLagState :: Core.Text}
                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                     Core.Generic)
                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                       Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                       Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                       Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern LagStateRequested :: LagState
pattern LagStateRequested = LagState' "requested"

pattern LagStatePending :: LagState
pattern LagStatePending = LagState' "pending"

pattern LagStateAvailable :: LagState
pattern LagStateAvailable = LagState' "available"

pattern LagStateDown :: LagState
pattern LagStateDown = LagState' "down"

pattern LagStateDeleting :: LagState
pattern LagStateDeleting = LagState' "deleting"

pattern LagStateDeleted :: LagState
pattern LagStateDeleted = LagState' "deleted"

pattern LagStateUnknown :: LagState
pattern LagStateUnknown = LagState' "unknown"

{-# COMPLETE 
  LagStateRequested,

  LagStatePending,

  LagStateAvailable,

  LagStateDown,

  LagStateDeleting,

  LagStateDeleted,

  LagStateUnknown,
  LagState'
  #-}
