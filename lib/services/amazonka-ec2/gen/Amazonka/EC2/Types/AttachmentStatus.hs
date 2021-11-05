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
-- Module      : Amazonka.EC2.Types.AttachmentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AttachmentStatus
  ( AttachmentStatus
      ( ..,
        AttachmentStatus_Attached,
        AttachmentStatus_Attaching,
        AttachmentStatus_Available,
        AttachmentStatus_Busy,
        AttachmentStatus_Detached,
        AttachmentStatus_Detaching
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

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

pattern AttachmentStatus_Attached :: AttachmentStatus
pattern AttachmentStatus_Attached = AttachmentStatus' "attached"

pattern AttachmentStatus_Attaching :: AttachmentStatus
pattern AttachmentStatus_Attaching = AttachmentStatus' "attaching"

pattern AttachmentStatus_Available :: AttachmentStatus
pattern AttachmentStatus_Available = AttachmentStatus' "available"

pattern AttachmentStatus_Busy :: AttachmentStatus
pattern AttachmentStatus_Busy = AttachmentStatus' "busy"

pattern AttachmentStatus_Detached :: AttachmentStatus
pattern AttachmentStatus_Detached = AttachmentStatus' "detached"

pattern AttachmentStatus_Detaching :: AttachmentStatus
pattern AttachmentStatus_Detaching = AttachmentStatus' "detaching"

{-# COMPLETE
  AttachmentStatus_Attached,
  AttachmentStatus_Attaching,
  AttachmentStatus_Available,
  AttachmentStatus_Busy,
  AttachmentStatus_Detached,
  AttachmentStatus_Detaching,
  AttachmentStatus'
  #-}
