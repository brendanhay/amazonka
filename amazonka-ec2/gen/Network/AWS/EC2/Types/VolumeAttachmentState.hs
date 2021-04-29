{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeAttachmentState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeAttachmentState
  ( VolumeAttachmentState
      ( ..,
        VolumeAttachmentState_Attached,
        VolumeAttachmentState_Attaching,
        VolumeAttachmentState_Busy,
        VolumeAttachmentState_Detached,
        VolumeAttachmentState_Detaching
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype VolumeAttachmentState = VolumeAttachmentState'
  { fromVolumeAttachmentState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern VolumeAttachmentState_Attached :: VolumeAttachmentState
pattern VolumeAttachmentState_Attached = VolumeAttachmentState' "attached"

pattern VolumeAttachmentState_Attaching :: VolumeAttachmentState
pattern VolumeAttachmentState_Attaching = VolumeAttachmentState' "attaching"

pattern VolumeAttachmentState_Busy :: VolumeAttachmentState
pattern VolumeAttachmentState_Busy = VolumeAttachmentState' "busy"

pattern VolumeAttachmentState_Detached :: VolumeAttachmentState
pattern VolumeAttachmentState_Detached = VolumeAttachmentState' "detached"

pattern VolumeAttachmentState_Detaching :: VolumeAttachmentState
pattern VolumeAttachmentState_Detaching = VolumeAttachmentState' "detaching"

{-# COMPLETE
  VolumeAttachmentState_Attached,
  VolumeAttachmentState_Attaching,
  VolumeAttachmentState_Busy,
  VolumeAttachmentState_Detached,
  VolumeAttachmentState_Detaching,
  VolumeAttachmentState'
  #-}
