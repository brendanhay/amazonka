-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceProtocol
  ( ContainerServiceProtocol
      ( ContainerServiceProtocol',
        CSPHTTP,
        CSPHTTPS,
        CSPTCP,
        CSPUdp
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ContainerServiceProtocol = ContainerServiceProtocol' Lude.Text
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

pattern CSPHTTP :: ContainerServiceProtocol
pattern CSPHTTP = ContainerServiceProtocol' "HTTP"

pattern CSPHTTPS :: ContainerServiceProtocol
pattern CSPHTTPS = ContainerServiceProtocol' "HTTPS"

pattern CSPTCP :: ContainerServiceProtocol
pattern CSPTCP = ContainerServiceProtocol' "TCP"

pattern CSPUdp :: ContainerServiceProtocol
pattern CSPUdp = ContainerServiceProtocol' "UDP"

{-# COMPLETE
  CSPHTTP,
  CSPHTTPS,
  CSPTCP,
  CSPUdp,
  ContainerServiceProtocol'
  #-}
