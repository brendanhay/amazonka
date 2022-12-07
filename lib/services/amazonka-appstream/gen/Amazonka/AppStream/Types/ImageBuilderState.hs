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
-- Module      : Amazonka.AppStream.Types.ImageBuilderState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.ImageBuilderState
  ( ImageBuilderState
      ( ..,
        ImageBuilderState_DELETING,
        ImageBuilderState_FAILED,
        ImageBuilderState_PENDING,
        ImageBuilderState_PENDING_QUALIFICATION,
        ImageBuilderState_REBOOTING,
        ImageBuilderState_RUNNING,
        ImageBuilderState_SNAPSHOTTING,
        ImageBuilderState_STOPPED,
        ImageBuilderState_STOPPING,
        ImageBuilderState_UPDATING,
        ImageBuilderState_UPDATING_AGENT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ImageBuilderState = ImageBuilderState'
  { fromImageBuilderState ::
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

pattern ImageBuilderState_DELETING :: ImageBuilderState
pattern ImageBuilderState_DELETING = ImageBuilderState' "DELETING"

pattern ImageBuilderState_FAILED :: ImageBuilderState
pattern ImageBuilderState_FAILED = ImageBuilderState' "FAILED"

pattern ImageBuilderState_PENDING :: ImageBuilderState
pattern ImageBuilderState_PENDING = ImageBuilderState' "PENDING"

pattern ImageBuilderState_PENDING_QUALIFICATION :: ImageBuilderState
pattern ImageBuilderState_PENDING_QUALIFICATION = ImageBuilderState' "PENDING_QUALIFICATION"

pattern ImageBuilderState_REBOOTING :: ImageBuilderState
pattern ImageBuilderState_REBOOTING = ImageBuilderState' "REBOOTING"

pattern ImageBuilderState_RUNNING :: ImageBuilderState
pattern ImageBuilderState_RUNNING = ImageBuilderState' "RUNNING"

pattern ImageBuilderState_SNAPSHOTTING :: ImageBuilderState
pattern ImageBuilderState_SNAPSHOTTING = ImageBuilderState' "SNAPSHOTTING"

pattern ImageBuilderState_STOPPED :: ImageBuilderState
pattern ImageBuilderState_STOPPED = ImageBuilderState' "STOPPED"

pattern ImageBuilderState_STOPPING :: ImageBuilderState
pattern ImageBuilderState_STOPPING = ImageBuilderState' "STOPPING"

pattern ImageBuilderState_UPDATING :: ImageBuilderState
pattern ImageBuilderState_UPDATING = ImageBuilderState' "UPDATING"

pattern ImageBuilderState_UPDATING_AGENT :: ImageBuilderState
pattern ImageBuilderState_UPDATING_AGENT = ImageBuilderState' "UPDATING_AGENT"

{-# COMPLETE
  ImageBuilderState_DELETING,
  ImageBuilderState_FAILED,
  ImageBuilderState_PENDING,
  ImageBuilderState_PENDING_QUALIFICATION,
  ImageBuilderState_REBOOTING,
  ImageBuilderState_RUNNING,
  ImageBuilderState_SNAPSHOTTING,
  ImageBuilderState_STOPPED,
  ImageBuilderState_STOPPING,
  ImageBuilderState_UPDATING,
  ImageBuilderState_UPDATING_AGENT,
  ImageBuilderState'
  #-}
