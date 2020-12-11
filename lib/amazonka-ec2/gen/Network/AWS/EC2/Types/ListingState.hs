-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ListingState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ListingState
  ( ListingState
      ( ListingState',
        LAvailable,
        LCancelled,
        LPending,
        LSold
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ListingState = ListingState' Lude.Text
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

pattern LAvailable :: ListingState
pattern LAvailable = ListingState' "available"

pattern LCancelled :: ListingState
pattern LCancelled = ListingState' "cancelled"

pattern LPending :: ListingState
pattern LPending = ListingState' "pending"

pattern LSold :: ListingState
pattern LSold = ListingState' "sold"

{-# COMPLETE
  LAvailable,
  LCancelled,
  LPending,
  LSold,
  ListingState'
  #-}
