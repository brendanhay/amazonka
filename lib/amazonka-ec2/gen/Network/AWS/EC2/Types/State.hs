{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        SfPendingAcceptance,
        SfPending,
        SfAvailable,
        SfDeleting,
        SfDeleted,
        SfRejected,
        SfFailed,
        SfExpired
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

pattern SfPendingAcceptance :: State
pattern SfPendingAcceptance = State' "PendingAcceptance"

pattern SfPending :: State
pattern SfPending = State' "Pending"

pattern SfAvailable :: State
pattern SfAvailable = State' "Available"

pattern SfDeleting :: State
pattern SfDeleting = State' "Deleting"

pattern SfDeleted :: State
pattern SfDeleted = State' "Deleted"

pattern SfRejected :: State
pattern SfRejected = State' "Rejected"

pattern SfFailed :: State
pattern SfFailed = State' "Failed"

pattern SfExpired :: State
pattern SfExpired = State' "Expired"

{-# COMPLETE
  SfPendingAcceptance,
  SfPending,
  SfAvailable,
  SfDeleting,
  SfDeleted,
  SfRejected,
  SfFailed,
  SfExpired,
  State'
  #-}
