{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        AttachmentStatusAAttaching,
        AttachmentStatusAAttached,
        AttachmentStatusADetaching,
        AttachmentStatusADetached,
        AttachmentStatusABusy,
        AttachmentStatusAAvailable,
        fromAttachmentStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AttachmentStatus = AttachmentStatus'
  { fromAttachmentStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern AttachmentStatusAAttaching :: AttachmentStatus
pattern AttachmentStatusAAttaching = AttachmentStatus' "attaching"

pattern AttachmentStatusAAttached :: AttachmentStatus
pattern AttachmentStatusAAttached = AttachmentStatus' "attached"

pattern AttachmentStatusADetaching :: AttachmentStatus
pattern AttachmentStatusADetaching = AttachmentStatus' "detaching"

pattern AttachmentStatusADetached :: AttachmentStatus
pattern AttachmentStatusADetached = AttachmentStatus' "detached"

pattern AttachmentStatusABusy :: AttachmentStatus
pattern AttachmentStatusABusy = AttachmentStatus' "busy"

pattern AttachmentStatusAAvailable :: AttachmentStatus
pattern AttachmentStatusAAvailable = AttachmentStatus' "available"

{-# COMPLETE
  AttachmentStatusAAttaching,
  AttachmentStatusAAttached,
  AttachmentStatusADetaching,
  AttachmentStatusADetached,
  AttachmentStatusABusy,
  AttachmentStatusAAvailable,
  AttachmentStatus'
  #-}
