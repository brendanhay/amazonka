-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerProtocol
  ( LoadBalancerProtocol
      ( LoadBalancerProtocol',
        HTTP,
        HTTPHTTPS
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LoadBalancerProtocol = LoadBalancerProtocol' Lude.Text
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

pattern HTTP :: LoadBalancerProtocol
pattern HTTP = LoadBalancerProtocol' "HTTP"

pattern HTTPHTTPS :: LoadBalancerProtocol
pattern HTTPHTTPS = LoadBalancerProtocol' "HTTP_HTTPS"

{-# COMPLETE
  HTTP,
  HTTPHTTPS,
  LoadBalancerProtocol'
  #-}
