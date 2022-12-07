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
-- Module      : Amazonka.MQ.Types.BrokerState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.BrokerState
  ( BrokerState
      ( ..,
        BrokerState_CREATION_FAILED,
        BrokerState_CREATION_IN_PROGRESS,
        BrokerState_CRITICAL_ACTION_REQUIRED,
        BrokerState_DELETION_IN_PROGRESS,
        BrokerState_REBOOT_IN_PROGRESS,
        BrokerState_RUNNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The broker\'s status.
newtype BrokerState = BrokerState'
  { fromBrokerState ::
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

pattern BrokerState_CREATION_FAILED :: BrokerState
pattern BrokerState_CREATION_FAILED = BrokerState' "CREATION_FAILED"

pattern BrokerState_CREATION_IN_PROGRESS :: BrokerState
pattern BrokerState_CREATION_IN_PROGRESS = BrokerState' "CREATION_IN_PROGRESS"

pattern BrokerState_CRITICAL_ACTION_REQUIRED :: BrokerState
pattern BrokerState_CRITICAL_ACTION_REQUIRED = BrokerState' "CRITICAL_ACTION_REQUIRED"

pattern BrokerState_DELETION_IN_PROGRESS :: BrokerState
pattern BrokerState_DELETION_IN_PROGRESS = BrokerState' "DELETION_IN_PROGRESS"

pattern BrokerState_REBOOT_IN_PROGRESS :: BrokerState
pattern BrokerState_REBOOT_IN_PROGRESS = BrokerState' "REBOOT_IN_PROGRESS"

pattern BrokerState_RUNNING :: BrokerState
pattern BrokerState_RUNNING = BrokerState' "RUNNING"

{-# COMPLETE
  BrokerState_CREATION_FAILED,
  BrokerState_CREATION_IN_PROGRESS,
  BrokerState_CRITICAL_ACTION_REQUIRED,
  BrokerState_DELETION_IN_PROGRESS,
  BrokerState_REBOOT_IN_PROGRESS,
  BrokerState_RUNNING,
  BrokerState'
  #-}
