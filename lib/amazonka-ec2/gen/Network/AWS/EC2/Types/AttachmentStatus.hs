-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AttachmentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AttachmentStatus
  ( AttachmentStatus
      ( AttachmentStatus',
        AAttached,
        AAttaching,
        AAvailable,
        ABusy,
        ADetached,
        ADetaching
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AttachmentStatus = AttachmentStatus' Lude.Text
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

pattern AAttached :: AttachmentStatus
pattern AAttached = AttachmentStatus' "attached"

pattern AAttaching :: AttachmentStatus
pattern AAttaching = AttachmentStatus' "attaching"

pattern AAvailable :: AttachmentStatus
pattern AAvailable = AttachmentStatus' "available"

pattern ABusy :: AttachmentStatus
pattern ABusy = AttachmentStatus' "busy"

pattern ADetached :: AttachmentStatus
pattern ADetached = AttachmentStatus' "detached"

pattern ADetaching :: AttachmentStatus
pattern ADetaching = AttachmentStatus' "detaching"

{-# COMPLETE
  AAttached,
  AAttaching,
  AAvailable,
  ABusy,
  ADetached,
  ADetaching,
  AttachmentStatus'
  #-}
