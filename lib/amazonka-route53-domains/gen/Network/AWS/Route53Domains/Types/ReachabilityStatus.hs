{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        Pending,
        Done,
        Expired
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

pattern Pending :: ReachabilityStatus
pattern Pending = ReachabilityStatus' "PENDING"

pattern Done :: ReachabilityStatus
pattern Done = ReachabilityStatus' "DONE"

pattern Expired :: ReachabilityStatus
pattern Expired = ReachabilityStatus' "EXPIRED"

{-# COMPLETE
  Pending,
  Done,
  Expired,
  ReachabilityStatus'
  #-}
