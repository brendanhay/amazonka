-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.State
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.State
  ( State
      ( State',
        SAvailable,
        SDeleted,
        SDeleting,
        SExpired,
        SFailed,
        SPending,
        SPendingAcceptance,
        SRejected
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype State = State' Lude.Text
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

pattern SAvailable :: State
pattern SAvailable = State' "Available"

pattern SDeleted :: State
pattern SDeleted = State' "Deleted"

pattern SDeleting :: State
pattern SDeleting = State' "Deleting"

pattern SExpired :: State
pattern SExpired = State' "Expired"

pattern SFailed :: State
pattern SFailed = State' "Failed"

pattern SPending :: State
pattern SPending = State' "Pending"

pattern SPendingAcceptance :: State
pattern SPendingAcceptance = State' "PendingAcceptance"

pattern SRejected :: State
pattern SRejected = State' "Rejected"

{-# COMPLETE
  SAvailable,
  SDeleted,
  SDeleting,
  SExpired,
  SFailed,
  SPending,
  SPendingAcceptance,
  SRejected,
  State'
  #-}
