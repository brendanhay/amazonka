{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.CommsProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.CommsProtocol
  ( CommsProtocol
    ( CommsProtocol'
    , CommsProtocolSip
    , CommsProtocolSips
    , CommsProtocolH323
    , fromCommsProtocol
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype CommsProtocol = CommsProtocol'{fromCommsProtocol ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern CommsProtocolSip :: CommsProtocol
pattern CommsProtocolSip = CommsProtocol' "SIP"

pattern CommsProtocolSips :: CommsProtocol
pattern CommsProtocolSips = CommsProtocol' "SIPS"

pattern CommsProtocolH323 :: CommsProtocol
pattern CommsProtocolH323 = CommsProtocol' "H323"

{-# COMPLETE 
  CommsProtocolSip,

  CommsProtocolSips,

  CommsProtocolH323,
  CommsProtocol'
  #-}
