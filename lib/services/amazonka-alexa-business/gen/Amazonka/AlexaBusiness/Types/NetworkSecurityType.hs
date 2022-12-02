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
-- Module      : Amazonka.AlexaBusiness.Types.NetworkSecurityType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.NetworkSecurityType
  ( NetworkSecurityType
      ( ..,
        NetworkSecurityType_OPEN,
        NetworkSecurityType_WEP,
        NetworkSecurityType_WPA2_ENTERPRISE,
        NetworkSecurityType_WPA2_PSK,
        NetworkSecurityType_WPA_PSK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NetworkSecurityType = NetworkSecurityType'
  { fromNetworkSecurityType ::
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

pattern NetworkSecurityType_OPEN :: NetworkSecurityType
pattern NetworkSecurityType_OPEN = NetworkSecurityType' "OPEN"

pattern NetworkSecurityType_WEP :: NetworkSecurityType
pattern NetworkSecurityType_WEP = NetworkSecurityType' "WEP"

pattern NetworkSecurityType_WPA2_ENTERPRISE :: NetworkSecurityType
pattern NetworkSecurityType_WPA2_ENTERPRISE = NetworkSecurityType' "WPA2_ENTERPRISE"

pattern NetworkSecurityType_WPA2_PSK :: NetworkSecurityType
pattern NetworkSecurityType_WPA2_PSK = NetworkSecurityType' "WPA2_PSK"

pattern NetworkSecurityType_WPA_PSK :: NetworkSecurityType
pattern NetworkSecurityType_WPA_PSK = NetworkSecurityType' "WPA_PSK"

{-# COMPLETE
  NetworkSecurityType_OPEN,
  NetworkSecurityType_WEP,
  NetworkSecurityType_WPA2_ENTERPRISE,
  NetworkSecurityType_WPA2_PSK,
  NetworkSecurityType_WPA_PSK,
  NetworkSecurityType'
  #-}
