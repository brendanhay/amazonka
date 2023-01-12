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
-- Module      : Amazonka.IoTWireless.Types.WirelessGatewayTaskStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WirelessGatewayTaskStatus
  ( WirelessGatewayTaskStatus
      ( ..,
        WirelessGatewayTaskStatus_COMPLETED,
        WirelessGatewayTaskStatus_FAILED,
        WirelessGatewayTaskStatus_FIRST_RETRY,
        WirelessGatewayTaskStatus_IN_PROGRESS,
        WirelessGatewayTaskStatus_PENDING,
        WirelessGatewayTaskStatus_SECOND_RETRY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WirelessGatewayTaskStatus = WirelessGatewayTaskStatus'
  { fromWirelessGatewayTaskStatus ::
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

pattern WirelessGatewayTaskStatus_COMPLETED :: WirelessGatewayTaskStatus
pattern WirelessGatewayTaskStatus_COMPLETED = WirelessGatewayTaskStatus' "COMPLETED"

pattern WirelessGatewayTaskStatus_FAILED :: WirelessGatewayTaskStatus
pattern WirelessGatewayTaskStatus_FAILED = WirelessGatewayTaskStatus' "FAILED"

pattern WirelessGatewayTaskStatus_FIRST_RETRY :: WirelessGatewayTaskStatus
pattern WirelessGatewayTaskStatus_FIRST_RETRY = WirelessGatewayTaskStatus' "FIRST_RETRY"

pattern WirelessGatewayTaskStatus_IN_PROGRESS :: WirelessGatewayTaskStatus
pattern WirelessGatewayTaskStatus_IN_PROGRESS = WirelessGatewayTaskStatus' "IN_PROGRESS"

pattern WirelessGatewayTaskStatus_PENDING :: WirelessGatewayTaskStatus
pattern WirelessGatewayTaskStatus_PENDING = WirelessGatewayTaskStatus' "PENDING"

pattern WirelessGatewayTaskStatus_SECOND_RETRY :: WirelessGatewayTaskStatus
pattern WirelessGatewayTaskStatus_SECOND_RETRY = WirelessGatewayTaskStatus' "SECOND_RETRY"

{-# COMPLETE
  WirelessGatewayTaskStatus_COMPLETED,
  WirelessGatewayTaskStatus_FAILED,
  WirelessGatewayTaskStatus_FIRST_RETRY,
  WirelessGatewayTaskStatus_IN_PROGRESS,
  WirelessGatewayTaskStatus_PENDING,
  WirelessGatewayTaskStatus_SECOND_RETRY,
  WirelessGatewayTaskStatus'
  #-}
