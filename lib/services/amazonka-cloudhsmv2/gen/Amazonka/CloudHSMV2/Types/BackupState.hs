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
-- Module      : Amazonka.CloudHSMV2.Types.BackupState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudHSMV2.Types.BackupState
  ( BackupState
      ( ..,
        BackupState_CREATE_IN_PROGRESS,
        BackupState_DELETED,
        BackupState_PENDING_DELETION,
        BackupState_READY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BackupState = BackupState'
  { fromBackupState ::
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

pattern BackupState_CREATE_IN_PROGRESS :: BackupState
pattern BackupState_CREATE_IN_PROGRESS = BackupState' "CREATE_IN_PROGRESS"

pattern BackupState_DELETED :: BackupState
pattern BackupState_DELETED = BackupState' "DELETED"

pattern BackupState_PENDING_DELETION :: BackupState
pattern BackupState_PENDING_DELETION = BackupState' "PENDING_DELETION"

pattern BackupState_READY :: BackupState
pattern BackupState_READY = BackupState' "READY"

{-# COMPLETE
  BackupState_CREATE_IN_PROGRESS,
  BackupState_DELETED,
  BackupState_PENDING_DELETION,
  BackupState_READY,
  BackupState'
  #-}
