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
-- Module      : Amazonka.DataBrew.Types.SessionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.SessionStatus
  ( SessionStatus
      ( ..,
        SessionStatus_ASSIGNED,
        SessionStatus_FAILED,
        SessionStatus_INITIALIZING,
        SessionStatus_PROVISIONING,
        SessionStatus_READY,
        SessionStatus_RECYCLING,
        SessionStatus_ROTATING,
        SessionStatus_TERMINATED,
        SessionStatus_TERMINATING,
        SessionStatus_UPDATING
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

pattern SessionStatus_ASSIGNED :: SessionStatus
pattern SessionStatus_ASSIGNED = SessionStatus' "ASSIGNED"

pattern SessionStatus_FAILED :: SessionStatus
pattern SessionStatus_FAILED = SessionStatus' "FAILED"

pattern SessionStatus_INITIALIZING :: SessionStatus
pattern SessionStatus_INITIALIZING = SessionStatus' "INITIALIZING"

pattern SessionStatus_PROVISIONING :: SessionStatus
pattern SessionStatus_PROVISIONING = SessionStatus' "PROVISIONING"

pattern SessionStatus_READY :: SessionStatus
pattern SessionStatus_READY = SessionStatus' "READY"

pattern SessionStatus_RECYCLING :: SessionStatus
pattern SessionStatus_RECYCLING = SessionStatus' "RECYCLING"

pattern SessionStatus_ROTATING :: SessionStatus
pattern SessionStatus_ROTATING = SessionStatus' "ROTATING"

pattern SessionStatus_TERMINATED :: SessionStatus
pattern SessionStatus_TERMINATED = SessionStatus' "TERMINATED"

pattern SessionStatus_TERMINATING :: SessionStatus
pattern SessionStatus_TERMINATING = SessionStatus' "TERMINATING"

pattern SessionStatus_UPDATING :: SessionStatus
pattern SessionStatus_UPDATING = SessionStatus' "UPDATING"

{-# COMPLETE
  SessionStatus_ASSIGNED,
  SessionStatus_FAILED,
  SessionStatus_INITIALIZING,
  SessionStatus_PROVISIONING,
  SessionStatus_READY,
  SessionStatus_RECYCLING,
  SessionStatus_ROTATING,
  SessionStatus_TERMINATED,
  SessionStatus_TERMINATING,
  SessionStatus_UPDATING,
  SessionStatus'
  #-}
