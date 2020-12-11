-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.ReachabilityStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.ReachabilityStatus
  ( ReachabilityStatus
      ( ReachabilityStatus',
        Done,
        Expired,
        Pending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ReachabilityStatus = ReachabilityStatus' Lude.Text
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

pattern Done :: ReachabilityStatus
pattern Done = ReachabilityStatus' "DONE"

pattern Expired :: ReachabilityStatus
pattern Expired = ReachabilityStatus' "EXPIRED"

pattern Pending :: ReachabilityStatus
pattern Pending = ReachabilityStatus' "PENDING"

{-# COMPLETE
  Done,
  Expired,
  Pending,
  ReachabilityStatus'
  #-}
