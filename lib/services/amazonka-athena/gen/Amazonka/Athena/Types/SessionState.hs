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
-- Module      : Amazonka.Athena.Types.SessionState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.SessionState
  ( SessionState
      ( ..,
        SessionState_BUSY,
        SessionState_CREATED,
        SessionState_CREATING,
        SessionState_DEGRADED,
        SessionState_FAILED,
        SessionState_IDLE,
        SessionState_TERMINATED,
        SessionState_TERMINATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SessionState = SessionState'
  { fromSessionState ::
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

pattern SessionState_BUSY :: SessionState
pattern SessionState_BUSY = SessionState' "BUSY"

pattern SessionState_CREATED :: SessionState
pattern SessionState_CREATED = SessionState' "CREATED"

pattern SessionState_CREATING :: SessionState
pattern SessionState_CREATING = SessionState' "CREATING"

pattern SessionState_DEGRADED :: SessionState
pattern SessionState_DEGRADED = SessionState' "DEGRADED"

pattern SessionState_FAILED :: SessionState
pattern SessionState_FAILED = SessionState' "FAILED"

pattern SessionState_IDLE :: SessionState
pattern SessionState_IDLE = SessionState' "IDLE"

pattern SessionState_TERMINATED :: SessionState
pattern SessionState_TERMINATED = SessionState' "TERMINATED"

pattern SessionState_TERMINATING :: SessionState
pattern SessionState_TERMINATING = SessionState' "TERMINATING"

{-# COMPLETE
  SessionState_BUSY,
  SessionState_CREATED,
  SessionState_CREATING,
  SessionState_DEGRADED,
  SessionState_FAILED,
  SessionState_IDLE,
  SessionState_TERMINATED,
  SessionState_TERMINATING,
  SessionState'
  #-}
