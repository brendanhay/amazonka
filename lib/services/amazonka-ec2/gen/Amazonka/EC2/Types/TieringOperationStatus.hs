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
-- Module      : Amazonka.EC2.Types.TieringOperationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TieringOperationStatus
  ( TieringOperationStatus
      ( ..,
        TieringOperationStatus_Archival_completed,
        TieringOperationStatus_Archival_failed,
        TieringOperationStatus_Archival_in_progress,
        TieringOperationStatus_Permanent_restore_completed,
        TieringOperationStatus_Permanent_restore_failed,
        TieringOperationStatus_Permanent_restore_in_progress,
        TieringOperationStatus_Temporary_restore_completed,
        TieringOperationStatus_Temporary_restore_failed,
        TieringOperationStatus_Temporary_restore_in_progress
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype TieringOperationStatus = TieringOperationStatus'
  { fromTieringOperationStatus ::
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

pattern TieringOperationStatus_Archival_completed :: TieringOperationStatus
pattern TieringOperationStatus_Archival_completed = TieringOperationStatus' "archival-completed"

pattern TieringOperationStatus_Archival_failed :: TieringOperationStatus
pattern TieringOperationStatus_Archival_failed = TieringOperationStatus' "archival-failed"

pattern TieringOperationStatus_Archival_in_progress :: TieringOperationStatus
pattern TieringOperationStatus_Archival_in_progress = TieringOperationStatus' "archival-in-progress"

pattern TieringOperationStatus_Permanent_restore_completed :: TieringOperationStatus
pattern TieringOperationStatus_Permanent_restore_completed = TieringOperationStatus' "permanent-restore-completed"

pattern TieringOperationStatus_Permanent_restore_failed :: TieringOperationStatus
pattern TieringOperationStatus_Permanent_restore_failed = TieringOperationStatus' "permanent-restore-failed"

pattern TieringOperationStatus_Permanent_restore_in_progress :: TieringOperationStatus
pattern TieringOperationStatus_Permanent_restore_in_progress = TieringOperationStatus' "permanent-restore-in-progress"

pattern TieringOperationStatus_Temporary_restore_completed :: TieringOperationStatus
pattern TieringOperationStatus_Temporary_restore_completed = TieringOperationStatus' "temporary-restore-completed"

pattern TieringOperationStatus_Temporary_restore_failed :: TieringOperationStatus
pattern TieringOperationStatus_Temporary_restore_failed = TieringOperationStatus' "temporary-restore-failed"

pattern TieringOperationStatus_Temporary_restore_in_progress :: TieringOperationStatus
pattern TieringOperationStatus_Temporary_restore_in_progress = TieringOperationStatus' "temporary-restore-in-progress"

{-# COMPLETE
  TieringOperationStatus_Archival_completed,
  TieringOperationStatus_Archival_failed,
  TieringOperationStatus_Archival_in_progress,
  TieringOperationStatus_Permanent_restore_completed,
  TieringOperationStatus_Permanent_restore_failed,
  TieringOperationStatus_Permanent_restore_in_progress,
  TieringOperationStatus_Temporary_restore_completed,
  TieringOperationStatus_Temporary_restore_failed,
  TieringOperationStatus_Temporary_restore_in_progress,
  TieringOperationStatus'
  #-}
