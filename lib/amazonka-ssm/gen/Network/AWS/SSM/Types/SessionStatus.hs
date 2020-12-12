{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SessionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SessionStatus
  ( SessionStatus
      ( SessionStatus',
        SSConnected,
        SSConnecting,
        SSDisconnected,
        SSFailed,
        SSTerminated,
        SSTerminating
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SessionStatus = SessionStatus' Lude.Text
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

pattern SSConnected :: SessionStatus
pattern SSConnected = SessionStatus' "Connected"

pattern SSConnecting :: SessionStatus
pattern SSConnecting = SessionStatus' "Connecting"

pattern SSDisconnected :: SessionStatus
pattern SSDisconnected = SessionStatus' "Disconnected"

pattern SSFailed :: SessionStatus
pattern SSFailed = SessionStatus' "Failed"

pattern SSTerminated :: SessionStatus
pattern SSTerminated = SessionStatus' "Terminated"

pattern SSTerminating :: SessionStatus
pattern SSTerminating = SessionStatus' "Terminating"

{-# COMPLETE
  SSConnected,
  SSConnecting,
  SSDisconnected,
  SSFailed,
  SSTerminated,
  SSTerminating,
  SessionStatus'
  #-}
