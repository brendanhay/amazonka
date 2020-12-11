-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Unit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Unit
  ( Unit
      ( Unit',
        Bits,
        Bytes,
        Packets,
        Requests
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Unit = Unit' Lude.Text
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

pattern Bits :: Unit
pattern Bits = Unit' "BITS"

pattern Bytes :: Unit
pattern Bytes = Unit' "BYTES"

pattern Packets :: Unit
pattern Packets = Unit' "PACKETS"

pattern Requests :: Unit
pattern Requests = Unit' "REQUESTS"

{-# COMPLETE
  Bits,
  Bytes,
  Packets,
  Requests,
  Unit'
  #-}
