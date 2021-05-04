{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | The status of the broker.
newtype BrokerState = BrokerState'
  { fromBrokerState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
