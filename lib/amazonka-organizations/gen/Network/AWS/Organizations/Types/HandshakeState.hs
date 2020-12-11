-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.HandshakeState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakeState
  ( HandshakeState
      ( HandshakeState',
        Accepted,
        Canceled,
        Declined,
        Expired,
        Open,
        Requested
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HandshakeState = HandshakeState' Lude.Text
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

pattern Accepted :: HandshakeState
pattern Accepted = HandshakeState' "ACCEPTED"

pattern Canceled :: HandshakeState
pattern Canceled = HandshakeState' "CANCELED"

pattern Declined :: HandshakeState
pattern Declined = HandshakeState' "DECLINED"

pattern Expired :: HandshakeState
pattern Expired = HandshakeState' "EXPIRED"

pattern Open :: HandshakeState
pattern Open = HandshakeState' "OPEN"

pattern Requested :: HandshakeState
pattern Requested = HandshakeState' "REQUESTED"

{-# COMPLETE
  Accepted,
  Canceled,
  Declined,
  Expired,
  Open,
  Requested,
  HandshakeState'
  #-}
