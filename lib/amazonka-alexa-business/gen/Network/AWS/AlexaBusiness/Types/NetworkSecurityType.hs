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
        Open,
        WPA2Enterprise,
        WPA2Psk,
        Wep,
        WpaPsk
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype NetworkSecurityType = NetworkSecurityType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Open :: NetworkSecurityType
pattern Open = NetworkSecurityType' "OPEN"

pattern WPA2Enterprise :: NetworkSecurityType
pattern WPA2Enterprise = NetworkSecurityType' "WPA2_ENTERPRISE"

pattern WPA2Psk :: NetworkSecurityType
pattern WPA2Psk = NetworkSecurityType' "WPA2_PSK"

pattern Wep :: NetworkSecurityType
pattern Wep = NetworkSecurityType' "WEP"

pattern WpaPsk :: NetworkSecurityType
pattern WpaPsk = NetworkSecurityType' "WPA_PSK"

{-# COMPLETE
  Open,
  WPA2Enterprise,
  WPA2Psk,
  Wep,
  WpaPsk,
  NetworkSecurityType'
  #-}
