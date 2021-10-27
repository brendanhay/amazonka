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
-- Module      : Network.AWS.SnowDeviceManagement.Types.AttachmentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SnowDeviceManagement.Types.AttachmentStatus
  ( AttachmentStatus
      ( ..,
        AttachmentStatus_ATTACHED,
        AttachmentStatus_ATTACHING,
        AttachmentStatus_DETACHED,
        AttachmentStatus_DETACHING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AttachmentStatus = AttachmentStatus'
  { fromAttachmentStatus ::
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

pattern AttachmentStatus_ATTACHED :: AttachmentStatus
pattern AttachmentStatus_ATTACHED = AttachmentStatus' "ATTACHED"

pattern AttachmentStatus_ATTACHING :: AttachmentStatus
pattern AttachmentStatus_ATTACHING = AttachmentStatus' "ATTACHING"

pattern AttachmentStatus_DETACHED :: AttachmentStatus
pattern AttachmentStatus_DETACHED = AttachmentStatus' "DETACHED"

pattern AttachmentStatus_DETACHING :: AttachmentStatus
pattern AttachmentStatus_DETACHING = AttachmentStatus' "DETACHING"

{-# COMPLETE
  AttachmentStatus_ATTACHED,
  AttachmentStatus_ATTACHING,
  AttachmentStatus_DETACHED,
  AttachmentStatus_DETACHING,
  AttachmentStatus'
  #-}
