-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeAttachmentState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeAttachmentState
  ( VolumeAttachmentState
      ( VolumeAttachmentState',
        VAttached,
        VAttaching,
        VBusy,
        VDetached,
        VDetaching
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype VolumeAttachmentState = VolumeAttachmentState' Lude.Text
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

pattern VAttached :: VolumeAttachmentState
pattern VAttached = VolumeAttachmentState' "attached"

pattern VAttaching :: VolumeAttachmentState
pattern VAttaching = VolumeAttachmentState' "attaching"

pattern VBusy :: VolumeAttachmentState
pattern VBusy = VolumeAttachmentState' "busy"

pattern VDetached :: VolumeAttachmentState
pattern VDetached = VolumeAttachmentState' "detached"

pattern VDetaching :: VolumeAttachmentState
pattern VDetaching = VolumeAttachmentState' "detaching"

{-# COMPLETE
  VAttached,
  VAttaching,
  VBusy,
  VDetached,
  VDetaching,
  VolumeAttachmentState'
  #-}
