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
-- Module      : Amazonka.SSM.Types.SessionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.SessionStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SessionStatus = SessionStatus'
  { fromSessionStatus ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
