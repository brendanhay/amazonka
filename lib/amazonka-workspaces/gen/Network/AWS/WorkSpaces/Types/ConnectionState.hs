{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ConnectionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.ConnectionState
  ( ConnectionState
    ( ConnectionState'
    , ConnectionStateConnected
    , ConnectionStateDisconnected
    , ConnectionStateUnknown
    , fromConnectionState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ConnectionState = ConnectionState'{fromConnectionState ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern ConnectionStateConnected :: ConnectionState
pattern ConnectionStateConnected = ConnectionState' "CONNECTED"

pattern ConnectionStateDisconnected :: ConnectionState
pattern ConnectionStateDisconnected = ConnectionState' "DISCONNECTED"

pattern ConnectionStateUnknown :: ConnectionState
pattern ConnectionStateUnknown = ConnectionState' "UNKNOWN"

{-# COMPLETE 
  ConnectionStateConnected,

  ConnectionStateDisconnected,

  ConnectionStateUnknown,
  ConnectionState'
  #-}
