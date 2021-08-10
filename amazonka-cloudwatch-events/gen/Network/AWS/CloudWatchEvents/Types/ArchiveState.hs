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
-- Module      : Network.AWS.CloudWatchEvents.Types.ArchiveState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ArchiveState
  ( ArchiveState
      ( ..,
        ArchiveState_CREATE_FAILED,
        ArchiveState_CREATING,
        ArchiveState_DISABLED,
        ArchiveState_ENABLED,
        ArchiveState_UPDATE_FAILED,
        ArchiveState_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ArchiveState = ArchiveState'
  { fromArchiveState ::
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

pattern ArchiveState_CREATE_FAILED :: ArchiveState
pattern ArchiveState_CREATE_FAILED = ArchiveState' "CREATE_FAILED"

pattern ArchiveState_CREATING :: ArchiveState
pattern ArchiveState_CREATING = ArchiveState' "CREATING"

pattern ArchiveState_DISABLED :: ArchiveState
pattern ArchiveState_DISABLED = ArchiveState' "DISABLED"

pattern ArchiveState_ENABLED :: ArchiveState
pattern ArchiveState_ENABLED = ArchiveState' "ENABLED"

pattern ArchiveState_UPDATE_FAILED :: ArchiveState
pattern ArchiveState_UPDATE_FAILED = ArchiveState' "UPDATE_FAILED"

pattern ArchiveState_UPDATING :: ArchiveState
pattern ArchiveState_UPDATING = ArchiveState' "UPDATING"

{-# COMPLETE
  ArchiveState_CREATE_FAILED,
  ArchiveState_CREATING,
  ArchiveState_DISABLED,
  ArchiveState_ENABLED,
  ArchiveState_UPDATE_FAILED,
  ArchiveState_UPDATING,
  ArchiveState'
  #-}
