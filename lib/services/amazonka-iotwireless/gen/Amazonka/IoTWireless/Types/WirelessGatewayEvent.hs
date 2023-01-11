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
-- Module      : Amazonka.IoTWireless.Types.WirelessGatewayEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WirelessGatewayEvent
  ( WirelessGatewayEvent
      ( ..,
        WirelessGatewayEvent_CUPS_Request,
        WirelessGatewayEvent_Certificate
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The event for a log message, if the log message is tied to a wireless
-- gateway.
newtype WirelessGatewayEvent = WirelessGatewayEvent'
  { fromWirelessGatewayEvent ::
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

pattern WirelessGatewayEvent_CUPS_Request :: WirelessGatewayEvent
pattern WirelessGatewayEvent_CUPS_Request = WirelessGatewayEvent' "CUPS_Request"

pattern WirelessGatewayEvent_Certificate :: WirelessGatewayEvent
pattern WirelessGatewayEvent_Certificate = WirelessGatewayEvent' "Certificate"

{-# COMPLETE
  WirelessGatewayEvent_CUPS_Request,
  WirelessGatewayEvent_Certificate,
  WirelessGatewayEvent'
  #-}
