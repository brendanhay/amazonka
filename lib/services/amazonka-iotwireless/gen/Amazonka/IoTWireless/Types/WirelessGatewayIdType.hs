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
-- Module      : Amazonka.IoTWireless.Types.WirelessGatewayIdType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WirelessGatewayIdType
  ( WirelessGatewayIdType
      ( ..,
        WirelessGatewayIdType_GatewayEui,
        WirelessGatewayIdType_ThingName,
        WirelessGatewayIdType_WirelessGatewayId
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WirelessGatewayIdType = WirelessGatewayIdType'
  { fromWirelessGatewayIdType ::
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

pattern WirelessGatewayIdType_GatewayEui :: WirelessGatewayIdType
pattern WirelessGatewayIdType_GatewayEui = WirelessGatewayIdType' "GatewayEui"

pattern WirelessGatewayIdType_ThingName :: WirelessGatewayIdType
pattern WirelessGatewayIdType_ThingName = WirelessGatewayIdType' "ThingName"

pattern WirelessGatewayIdType_WirelessGatewayId :: WirelessGatewayIdType
pattern WirelessGatewayIdType_WirelessGatewayId = WirelessGatewayIdType' "WirelessGatewayId"

{-# COMPLETE
  WirelessGatewayIdType_GatewayEui,
  WirelessGatewayIdType_ThingName,
  WirelessGatewayIdType_WirelessGatewayId,
  WirelessGatewayIdType'
  #-}
