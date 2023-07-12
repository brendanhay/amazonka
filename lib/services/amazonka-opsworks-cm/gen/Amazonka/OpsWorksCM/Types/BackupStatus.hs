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
-- Module      : Amazonka.OpsWorksCM.Types.BackupStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorksCM.Types.BackupStatus
  ( BackupStatus
      ( ..,
        BackupStatus_DELETING,
        BackupStatus_FAILED,
        BackupStatus_IN_PROGRESS,
        BackupStatus_OK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BackupStatus = BackupStatus'
  { fromBackupStatus ::
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

pattern BackupStatus_DELETING :: BackupStatus
pattern BackupStatus_DELETING = BackupStatus' "DELETING"

pattern BackupStatus_FAILED :: BackupStatus
pattern BackupStatus_FAILED = BackupStatus' "FAILED"

pattern BackupStatus_IN_PROGRESS :: BackupStatus
pattern BackupStatus_IN_PROGRESS = BackupStatus' "IN_PROGRESS"

pattern BackupStatus_OK :: BackupStatus
pattern BackupStatus_OK = BackupStatus' "OK"

{-# COMPLETE
  BackupStatus_DELETING,
  BackupStatus_FAILED,
  BackupStatus_IN_PROGRESS,
  BackupStatus_OK,
  BackupStatus'
  #-}
