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
-- Module      : Amazonka.IoTWireless.Types.IdentifierType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.IdentifierType
  ( IdentifierType
      ( ..,
        IdentifierType_DevEui,
        IdentifierType_GatewayEui,
        IdentifierType_PartnerAccountId,
        IdentifierType_WirelessDeviceId,
        IdentifierType_WirelessGatewayId
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype IdentifierType = IdentifierType'
  { fromIdentifierType ::
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

pattern IdentifierType_DevEui :: IdentifierType
pattern IdentifierType_DevEui = IdentifierType' "DevEui"

pattern IdentifierType_GatewayEui :: IdentifierType
pattern IdentifierType_GatewayEui = IdentifierType' "GatewayEui"

pattern IdentifierType_PartnerAccountId :: IdentifierType
pattern IdentifierType_PartnerAccountId = IdentifierType' "PartnerAccountId"

pattern IdentifierType_WirelessDeviceId :: IdentifierType
pattern IdentifierType_WirelessDeviceId = IdentifierType' "WirelessDeviceId"

pattern IdentifierType_WirelessGatewayId :: IdentifierType
pattern IdentifierType_WirelessGatewayId = IdentifierType' "WirelessGatewayId"

{-# COMPLETE
  IdentifierType_DevEui,
  IdentifierType_GatewayEui,
  IdentifierType_PartnerAccountId,
  IdentifierType_WirelessDeviceId,
  IdentifierType_WirelessGatewayId,
  IdentifierType'
  #-}
