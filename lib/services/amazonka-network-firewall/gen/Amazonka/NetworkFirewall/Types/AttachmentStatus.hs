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
-- Module      : Amazonka.NetworkFirewall.Types.AttachmentStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.AttachmentStatus
  ( AttachmentStatus
      ( ..,
        AttachmentStatus_CREATING,
        AttachmentStatus_DELETING,
        AttachmentStatus_READY,
        AttachmentStatus_SCALING
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
