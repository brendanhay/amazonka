{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeAttachmentState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VolumeAttachmentState
  ( VolumeAttachmentState
    ( VolumeAttachmentState'
    , VolumeAttachmentStateVAttaching
    , VolumeAttachmentStateVAttached
    , VolumeAttachmentStateVDetaching
    , VolumeAttachmentStateVDetached
    , VolumeAttachmentStateVBusy
    , fromVolumeAttachmentState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype VolumeAttachmentState = VolumeAttachmentState'{fromVolumeAttachmentState
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern VolumeAttachmentStateVAttaching :: VolumeAttachmentState
pattern VolumeAttachmentStateVAttaching = VolumeAttachmentState' "attaching"

pattern VolumeAttachmentStateVAttached :: VolumeAttachmentState
pattern VolumeAttachmentStateVAttached = VolumeAttachmentState' "attached"

pattern VolumeAttachmentStateVDetaching :: VolumeAttachmentState
pattern VolumeAttachmentStateVDetaching = VolumeAttachmentState' "detaching"

pattern VolumeAttachmentStateVDetached :: VolumeAttachmentState
pattern VolumeAttachmentStateVDetached = VolumeAttachmentState' "detached"

pattern VolumeAttachmentStateVBusy :: VolumeAttachmentState
pattern VolumeAttachmentStateVBusy = VolumeAttachmentState' "busy"

{-# COMPLETE 
  VolumeAttachmentStateVAttaching,

  VolumeAttachmentStateVAttached,

  VolumeAttachmentStateVDetaching,

  VolumeAttachmentStateVDetached,

  VolumeAttachmentStateVBusy,
  VolumeAttachmentState'
  #-}
