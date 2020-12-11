-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.HTTPVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.HTTPVersion
  ( HTTPVersion
      ( HTTPVersion',
        HTTP1_1,
        HTTP2
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HTTPVersion = HTTPVersion' Lude.Text
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

pattern HTTP1_1 :: HTTPVersion
pattern HTTP1_1 = HTTPVersion' "http1.1"

pattern HTTP2 :: HTTPVersion
pattern HTTP2 = HTTPVersion' "http2"

{-# COMPLETE
  HTTP1_1,
  HTTP2,
  HTTPVersion'
  #-}
