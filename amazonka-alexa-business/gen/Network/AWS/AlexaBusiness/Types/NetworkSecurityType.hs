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
-- Module      : Network.AWS.AlexaBusiness.Types.NetworkSecurityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.NetworkSecurityType
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype NetworkSecurityType = NetworkSecurityType'
  { fromNetworkSecurityType ::
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
