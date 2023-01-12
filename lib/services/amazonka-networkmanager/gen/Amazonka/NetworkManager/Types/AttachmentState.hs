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
-- Module      : Amazonka.NetworkManager.Types.AttachmentState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.AttachmentState
  ( AttachmentState
      ( ..,
        AttachmentState_AVAILABLE,
        AttachmentState_CREATING,
        AttachmentState_DELETING,
        AttachmentState_FAILED,
        AttachmentState_PENDING_ATTACHMENT_ACCEPTANCE,
        AttachmentState_PENDING_NETWORK_UPDATE,
        AttachmentState_PENDING_TAG_ACCEPTANCE,
        AttachmentState_REJECTED,
        AttachmentState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AttachmentState = AttachmentState'
  { fromAttachmentState ::
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

pattern AttachmentState_AVAILABLE :: AttachmentState
pattern AttachmentState_AVAILABLE = AttachmentState' "AVAILABLE"

pattern AttachmentState_CREATING :: AttachmentState
pattern AttachmentState_CREATING = AttachmentState' "CREATING"

pattern AttachmentState_DELETING :: AttachmentState
pattern AttachmentState_DELETING = AttachmentState' "DELETING"

pattern AttachmentState_FAILED :: AttachmentState
pattern AttachmentState_FAILED = AttachmentState' "FAILED"

pattern AttachmentState_PENDING_ATTACHMENT_ACCEPTANCE :: AttachmentState
pattern AttachmentState_PENDING_ATTACHMENT_ACCEPTANCE = AttachmentState' "PENDING_ATTACHMENT_ACCEPTANCE"

pattern AttachmentState_PENDING_NETWORK_UPDATE :: AttachmentState
pattern AttachmentState_PENDING_NETWORK_UPDATE = AttachmentState' "PENDING_NETWORK_UPDATE"

pattern AttachmentState_PENDING_TAG_ACCEPTANCE :: AttachmentState
pattern AttachmentState_PENDING_TAG_ACCEPTANCE = AttachmentState' "PENDING_TAG_ACCEPTANCE"

pattern AttachmentState_REJECTED :: AttachmentState
pattern AttachmentState_REJECTED = AttachmentState' "REJECTED"

pattern AttachmentState_UPDATING :: AttachmentState
pattern AttachmentState_UPDATING = AttachmentState' "UPDATING"

{-# COMPLETE
  AttachmentState_AVAILABLE,
  AttachmentState_CREATING,
  AttachmentState_DELETING,
  AttachmentState_FAILED,
  AttachmentState_PENDING_ATTACHMENT_ACCEPTANCE,
  AttachmentState_PENDING_NETWORK_UPDATE,
  AttachmentState_PENDING_TAG_ACCEPTANCE,
  AttachmentState_REJECTED,
  AttachmentState_UPDATING,
  AttachmentState'
  #-}
