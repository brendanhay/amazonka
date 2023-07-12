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
-- Module      : Amazonka.Glue.Types.SessionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SessionStatus
  ( SessionStatus
      ( ..,
        SessionStatus_FAILED,
        SessionStatus_PROVISIONING,
        SessionStatus_READY,
        SessionStatus_STOPPED,
        SessionStatus_STOPPING,
        SessionStatus_TIMEOUT
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

pattern SessionStatus_FAILED :: SessionStatus
pattern SessionStatus_FAILED = SessionStatus' "FAILED"

pattern SessionStatus_PROVISIONING :: SessionStatus
pattern SessionStatus_PROVISIONING = SessionStatus' "PROVISIONING"

pattern SessionStatus_READY :: SessionStatus
pattern SessionStatus_READY = SessionStatus' "READY"

pattern SessionStatus_STOPPED :: SessionStatus
pattern SessionStatus_STOPPED = SessionStatus' "STOPPED"

pattern SessionStatus_STOPPING :: SessionStatus
pattern SessionStatus_STOPPING = SessionStatus' "STOPPING"

pattern SessionStatus_TIMEOUT :: SessionStatus
pattern SessionStatus_TIMEOUT = SessionStatus' "TIMEOUT"

{-# COMPLETE
  SessionStatus_FAILED,
  SessionStatus_PROVISIONING,
  SessionStatus_READY,
  SessionStatus_STOPPED,
  SessionStatus_STOPPING,
  SessionStatus_TIMEOUT,
  SessionStatus'
  #-}
