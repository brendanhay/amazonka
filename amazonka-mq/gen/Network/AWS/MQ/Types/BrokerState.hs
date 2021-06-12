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
-- Module      : Network.AWS.MQ.Types.BrokerState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.BrokerState
  ( BrokerState
      ( ..,
        BrokerState_CREATION_FAILED,
        BrokerState_CREATION_IN_PROGRESS,
        BrokerState_DELETION_IN_PROGRESS,
        BrokerState_REBOOT_IN_PROGRESS,
        BrokerState_RUNNING
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The status of the broker.
newtype BrokerState = BrokerState'
  { fromBrokerState ::
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

pattern BrokerState_CREATION_FAILED :: BrokerState
pattern BrokerState_CREATION_FAILED = BrokerState' "CREATION_FAILED"

pattern BrokerState_CREATION_IN_PROGRESS :: BrokerState
pattern BrokerState_CREATION_IN_PROGRESS = BrokerState' "CREATION_IN_PROGRESS"

pattern BrokerState_DELETION_IN_PROGRESS :: BrokerState
pattern BrokerState_DELETION_IN_PROGRESS = BrokerState' "DELETION_IN_PROGRESS"

pattern BrokerState_REBOOT_IN_PROGRESS :: BrokerState
pattern BrokerState_REBOOT_IN_PROGRESS = BrokerState' "REBOOT_IN_PROGRESS"

pattern BrokerState_RUNNING :: BrokerState
pattern BrokerState_RUNNING = BrokerState' "RUNNING"

{-# COMPLETE
  BrokerState_CREATION_FAILED,
  BrokerState_CREATION_IN_PROGRESS,
  BrokerState_DELETION_IN_PROGRESS,
  BrokerState_REBOOT_IN_PROGRESS,
  BrokerState_RUNNING,
  BrokerState'
  #-}
