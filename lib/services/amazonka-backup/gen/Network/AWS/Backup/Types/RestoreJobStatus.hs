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
-- Module      : Network.AWS.Backup.Types.RestoreJobStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Backup.Types.RestoreJobStatus
  ( RestoreJobStatus
      ( ..,
        RestoreJobStatus_ABORTED,
        RestoreJobStatus_COMPLETED,
        RestoreJobStatus_FAILED,
        RestoreJobStatus_PENDING,
        RestoreJobStatus_RUNNING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RestoreJobStatus = RestoreJobStatus'
  { fromRestoreJobStatus ::
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

pattern RestoreJobStatus_ABORTED :: RestoreJobStatus
pattern RestoreJobStatus_ABORTED = RestoreJobStatus' "ABORTED"

pattern RestoreJobStatus_COMPLETED :: RestoreJobStatus
pattern RestoreJobStatus_COMPLETED = RestoreJobStatus' "COMPLETED"

pattern RestoreJobStatus_FAILED :: RestoreJobStatus
pattern RestoreJobStatus_FAILED = RestoreJobStatus' "FAILED"

pattern RestoreJobStatus_PENDING :: RestoreJobStatus
pattern RestoreJobStatus_PENDING = RestoreJobStatus' "PENDING"

pattern RestoreJobStatus_RUNNING :: RestoreJobStatus
pattern RestoreJobStatus_RUNNING = RestoreJobStatus' "RUNNING"

{-# COMPLETE
  RestoreJobStatus_ABORTED,
  RestoreJobStatus_COMPLETED,
  RestoreJobStatus_FAILED,
  RestoreJobStatus_PENDING,
  RestoreJobStatus_RUNNING,
  RestoreJobStatus'
  #-}
