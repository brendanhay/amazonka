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
-- Module      : Amazonka.SnowDeviceManagement.Types.AttachmentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.AttachmentStatus
  ( AttachmentStatus
      ( ..,
        AttachmentStatus_ATTACHED,
        AttachmentStatus_ATTACHING,
        AttachmentStatus_DETACHED,
        AttachmentStatus_DETACHING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AttachmentStatus = AttachmentStatus'
  { fromAttachmentStatus ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
