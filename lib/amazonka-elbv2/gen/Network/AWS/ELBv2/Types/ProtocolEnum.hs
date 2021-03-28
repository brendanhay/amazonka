{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.ProtocolEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.ProtocolEnum
  ( ProtocolEnum
    ( ProtocolEnum'
    , ProtocolEnumHttp
    , ProtocolEnumHttps
    , ProtocolEnumTcp
    , ProtocolEnumTls
    , ProtocolEnumUdp
    , ProtocolEnumTcpUdp
    , ProtocolEnumGeneve
    , fromProtocolEnum
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ProtocolEnum = ProtocolEnum'{fromProtocolEnum :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern ProtocolEnumHttp :: ProtocolEnum
pattern ProtocolEnumHttp = ProtocolEnum' "HTTP"

pattern ProtocolEnumHttps :: ProtocolEnum
pattern ProtocolEnumHttps = ProtocolEnum' "HTTPS"

pattern ProtocolEnumTcp :: ProtocolEnum
pattern ProtocolEnumTcp = ProtocolEnum' "TCP"

pattern ProtocolEnumTls :: ProtocolEnum
pattern ProtocolEnumTls = ProtocolEnum' "TLS"

pattern ProtocolEnumUdp :: ProtocolEnum
pattern ProtocolEnumUdp = ProtocolEnum' "UDP"

pattern ProtocolEnumTcpUdp :: ProtocolEnum
pattern ProtocolEnumTcpUdp = ProtocolEnum' "TCP_UDP"

pattern ProtocolEnumGeneve :: ProtocolEnum
pattern ProtocolEnumGeneve = ProtocolEnum' "GENEVE"

{-# COMPLETE 
  ProtocolEnumHttp,

  ProtocolEnumHttps,

  ProtocolEnumTcp,

  ProtocolEnumTls,

  ProtocolEnumUdp,

  ProtocolEnumTcpUdp,

  ProtocolEnumGeneve,
  ProtocolEnum'
  #-}
