{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.NetworkSecurityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.NetworkSecurityType
  ( NetworkSecurityType
      ( NetworkSecurityType',
        NetworkSecurityTypeOpen,
        NetworkSecurityTypeWep,
        NetworkSecurityTypeWpaPsk,
        NetworkSecurityTypeWPA2Psk,
        NetworkSecurityTypeWPA2Enterprise,
        fromNetworkSecurityType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype NetworkSecurityType = NetworkSecurityType'
  { fromNetworkSecurityType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern NetworkSecurityTypeOpen :: NetworkSecurityType
pattern NetworkSecurityTypeOpen = NetworkSecurityType' "OPEN"

pattern NetworkSecurityTypeWep :: NetworkSecurityType
pattern NetworkSecurityTypeWep = NetworkSecurityType' "WEP"

pattern NetworkSecurityTypeWpaPsk :: NetworkSecurityType
pattern NetworkSecurityTypeWpaPsk = NetworkSecurityType' "WPA_PSK"

pattern NetworkSecurityTypeWPA2Psk :: NetworkSecurityType
pattern NetworkSecurityTypeWPA2Psk = NetworkSecurityType' "WPA2_PSK"

pattern NetworkSecurityTypeWPA2Enterprise :: NetworkSecurityType
pattern NetworkSecurityTypeWPA2Enterprise = NetworkSecurityType' "WPA2_ENTERPRISE"

{-# COMPLETE
  NetworkSecurityTypeOpen,
  NetworkSecurityTypeWep,
  NetworkSecurityTypeWpaPsk,
  NetworkSecurityTypeWPA2Psk,
  NetworkSecurityTypeWPA2Enterprise,
  NetworkSecurityType'
  #-}
