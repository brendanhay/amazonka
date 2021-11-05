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
-- Module      : Network.AWS.NetworkFirewall.Types.AttachmentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.NetworkFirewall.Types.AttachmentStatus
  ( AttachmentStatus
      ( ..,
        AttachmentStatus_CREATING,
        AttachmentStatus_DELETING,
        AttachmentStatus_READY,
        AttachmentStatus_SCALING
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

pattern AttachmentStatus_CREATING :: AttachmentStatus
pattern AttachmentStatus_CREATING = AttachmentStatus' "CREATING"

pattern AttachmentStatus_DELETING :: AttachmentStatus
pattern AttachmentStatus_DELETING = AttachmentStatus' "DELETING"

pattern AttachmentStatus_READY :: AttachmentStatus
pattern AttachmentStatus_READY = AttachmentStatus' "READY"

pattern AttachmentStatus_SCALING :: AttachmentStatus
pattern AttachmentStatus_SCALING = AttachmentStatus' "SCALING"

{-# COMPLETE
  AttachmentStatus_CREATING,
  AttachmentStatus_DELETING,
  AttachmentStatus_READY,
  AttachmentStatus_SCALING,
  AttachmentStatus'
  #-}
