-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfaceType
  ( NetworkInterfaceType
      ( NetworkInterfaceType',
        NITEfa,
        NITInterface,
        NITNatGateway
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype NetworkInterfaceType = NetworkInterfaceType' Lude.Text
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

pattern NITEfa :: NetworkInterfaceType
pattern NITEfa = NetworkInterfaceType' "efa"

pattern NITInterface :: NetworkInterfaceType
pattern NITInterface = NetworkInterfaceType' "interface"

pattern NITNatGateway :: NetworkInterfaceType
pattern NITNatGateway = NetworkInterfaceType' "natGateway"

{-# COMPLETE
  NITEfa,
  NITInterface,
  NITNatGateway,
  NetworkInterfaceType'
  #-}
