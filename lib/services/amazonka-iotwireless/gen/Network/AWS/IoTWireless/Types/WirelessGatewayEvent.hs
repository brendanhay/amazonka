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
-- Module      : Network.AWS.IoTWireless.Types.WirelessGatewayEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTWireless.Types.WirelessGatewayEvent
  ( WirelessGatewayEvent
      ( ..,
        WirelessGatewayEvent_CUPS_Request,
        WirelessGatewayEvent_Certificate
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The event for a log message, if the log message is tied to a wireless
-- gateway.
newtype WirelessGatewayEvent = WirelessGatewayEvent'
  { fromWirelessGatewayEvent ::
      Core.Text
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

pattern WirelessGatewayEvent_CUPS_Request :: WirelessGatewayEvent
pattern WirelessGatewayEvent_CUPS_Request = WirelessGatewayEvent' "CUPS_Request"

pattern WirelessGatewayEvent_Certificate :: WirelessGatewayEvent
pattern WirelessGatewayEvent_Certificate = WirelessGatewayEvent' "Certificate"

{-# COMPLETE
  WirelessGatewayEvent_CUPS_Request,
  WirelessGatewayEvent_Certificate,
  WirelessGatewayEvent'
  #-}
