-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.SSLProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.SSLProtocol
  ( SSLProtocol
      ( SSLProtocol',
        SSLV3,
        TLSV1,
        TLSV1_1,
        TLSV1_2
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SSLProtocol = SSLProtocol' Lude.Text
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

pattern SSLV3 :: SSLProtocol
pattern SSLV3 = SSLProtocol' "SSLv3"

pattern TLSV1 :: SSLProtocol
pattern TLSV1 = SSLProtocol' "TLSv1"

pattern TLSV1_1 :: SSLProtocol
pattern TLSV1_1 = SSLProtocol' "TLSv1.1"

pattern TLSV1_2 :: SSLProtocol
pattern TLSV1_2 = SSLProtocol' "TLSv1.2"

{-# COMPLETE
  SSLV3,
  TLSV1,
  TLSV1_1,
  TLSV1_2,
  SSLProtocol'
  #-}
