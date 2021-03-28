{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SessionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.SessionStatus
  ( SessionStatus
    ( SessionStatus'
    , SessionStatusConnected
    , SessionStatusConnecting
    , SessionStatusDisconnected
    , SessionStatusTerminated
    , SessionStatusTerminating
    , SessionStatusFailed
    , fromSessionStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype SessionStatus = SessionStatus'{fromSessionStatus ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern SessionStatusConnected :: SessionStatus
pattern SessionStatusConnected = SessionStatus' "Connected"

pattern SessionStatusConnecting :: SessionStatus
pattern SessionStatusConnecting = SessionStatus' "Connecting"

pattern SessionStatusDisconnected :: SessionStatus
pattern SessionStatusDisconnected = SessionStatus' "Disconnected"

pattern SessionStatusTerminated :: SessionStatus
pattern SessionStatusTerminated = SessionStatus' "Terminated"

pattern SessionStatusTerminating :: SessionStatus
pattern SessionStatusTerminating = SessionStatus' "Terminating"

pattern SessionStatusFailed :: SessionStatus
pattern SessionStatusFailed = SessionStatus' "Failed"

{-# COMPLETE 
  SessionStatusConnected,

  SessionStatusConnecting,

  SessionStatusDisconnected,

  SessionStatusTerminated,

  SessionStatusTerminating,

  SessionStatusFailed,
  SessionStatus'
  #-}
