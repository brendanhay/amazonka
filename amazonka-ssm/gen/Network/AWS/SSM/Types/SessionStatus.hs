{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SessionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SessionStatus
  ( SessionStatus
      ( ..,
        SessionStatus_Connected,
        SessionStatus_Connecting,
        SessionStatus_Disconnected,
        SessionStatus_Failed,
        SessionStatus_Terminated,
        SessionStatus_Terminating
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype SessionStatus = SessionStatus'
  { fromSessionStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern SessionStatus_Connected :: SessionStatus
pattern SessionStatus_Connected = SessionStatus' "Connected"

pattern SessionStatus_Connecting :: SessionStatus
pattern SessionStatus_Connecting = SessionStatus' "Connecting"

pattern SessionStatus_Disconnected :: SessionStatus
pattern SessionStatus_Disconnected = SessionStatus' "Disconnected"

pattern SessionStatus_Failed :: SessionStatus
pattern SessionStatus_Failed = SessionStatus' "Failed"

pattern SessionStatus_Terminated :: SessionStatus
pattern SessionStatus_Terminated = SessionStatus' "Terminated"

pattern SessionStatus_Terminating :: SessionStatus
pattern SessionStatus_Terminating = SessionStatus' "Terminating"

{-# COMPLETE
  SessionStatus_Connected,
  SessionStatus_Connecting,
  SessionStatus_Disconnected,
  SessionStatus_Failed,
  SessionStatus_Terminated,
  SessionStatus_Terminating,
  SessionStatus'
  #-}
