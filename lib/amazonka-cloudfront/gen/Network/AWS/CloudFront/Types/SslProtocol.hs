{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.SslProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.SslProtocol
  ( SslProtocol
    ( SslProtocol'
    , SslProtocolSSLV3
    , SslProtocolTLSV1
    , SslProtocolTLSV1_1
    , SslProtocolTLSV1_2
    , fromSslProtocol
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype SslProtocol = SslProtocol'{fromSslProtocol :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern SslProtocolSSLV3 :: SslProtocol
pattern SslProtocolSSLV3 = SslProtocol' "SSLv3"

pattern SslProtocolTLSV1 :: SslProtocol
pattern SslProtocolTLSV1 = SslProtocol' "TLSv1"

pattern SslProtocolTLSV1_1 :: SslProtocol
pattern SslProtocolTLSV1_1 = SslProtocol' "TLSv1.1"

pattern SslProtocolTLSV1_2 :: SslProtocol
pattern SslProtocolTLSV1_2 = SslProtocol' "TLSv1.2"

{-# COMPLETE 
  SslProtocolSSLV3,

  SslProtocolTLSV1,

  SslProtocolTLSV1_1,

  SslProtocolTLSV1_2,
  SslProtocol'
  #-}
