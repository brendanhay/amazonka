{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.SSLSupportMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.SSLSupportMethod
  ( SSLSupportMethod
      ( SSLSupportMethod',
        SSLSupportMethodSniOnly,
        SSLSupportMethodVip,
        SSLSupportMethodStaticIp,
        fromSSLSupportMethod
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype SSLSupportMethod = SSLSupportMethod'
  { fromSSLSupportMethod ::
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

pattern SSLSupportMethodSniOnly :: SSLSupportMethod
pattern SSLSupportMethodSniOnly = SSLSupportMethod' "sni-only"

pattern SSLSupportMethodVip :: SSLSupportMethod
pattern SSLSupportMethodVip = SSLSupportMethod' "vip"

pattern SSLSupportMethodStaticIp :: SSLSupportMethod
pattern SSLSupportMethodStaticIp = SSLSupportMethod' "static-ip"

{-# COMPLETE
  SSLSupportMethodSniOnly,
  SSLSupportMethodVip,
  SSLSupportMethodStaticIp,
  SSLSupportMethod'
  #-}
