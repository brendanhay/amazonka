{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        ContainerServiceProtocolHttp,
        ContainerServiceProtocolHttps,
        ContainerServiceProtocolTcp,
        ContainerServiceProtocolUdp,
        fromContainerServiceProtocol
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ContainerServiceProtocol = ContainerServiceProtocol'
  { fromContainerServiceProtocol ::
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

pattern ContainerServiceProtocolHttp :: ContainerServiceProtocol
pattern ContainerServiceProtocolHttp = ContainerServiceProtocol' "HTTP"

pattern ContainerServiceProtocolHttps :: ContainerServiceProtocol
pattern ContainerServiceProtocolHttps = ContainerServiceProtocol' "HTTPS"

pattern ContainerServiceProtocolTcp :: ContainerServiceProtocol
pattern ContainerServiceProtocolTcp = ContainerServiceProtocol' "TCP"

pattern ContainerServiceProtocolUdp :: ContainerServiceProtocol
pattern ContainerServiceProtocolUdp = ContainerServiceProtocol' "UDP"

{-# COMPLETE
  ContainerServiceProtocolHttp,
  ContainerServiceProtocolHttps,
  ContainerServiceProtocolTcp,
  ContainerServiceProtocolUdp,
  ContainerServiceProtocol'
  #-}
