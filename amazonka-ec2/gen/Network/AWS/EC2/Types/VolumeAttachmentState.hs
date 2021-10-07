{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype VolumeAttachmentState = VolumeAttachmentState'
  { fromVolumeAttachmentState ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
