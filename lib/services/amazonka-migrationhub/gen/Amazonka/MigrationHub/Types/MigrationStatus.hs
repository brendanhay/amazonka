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
-- Module      : Amazonka.MigrationHub.Types.MigrationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHub.Types.MigrationStatus
  ( MigrationStatus
      ( ..,
        MigrationStatus_COMPLETED,
        MigrationStatus_FAILED,
        MigrationStatus_IN_PROGRESS,
        MigrationStatus_NOT_STARTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MigrationStatus = MigrationStatus'
  { fromMigrationStatus ::
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

pattern MigrationStatus_COMPLETED :: MigrationStatus
pattern MigrationStatus_COMPLETED = MigrationStatus' "COMPLETED"

pattern MigrationStatus_FAILED :: MigrationStatus
pattern MigrationStatus_FAILED = MigrationStatus' "FAILED"

pattern MigrationStatus_IN_PROGRESS :: MigrationStatus
pattern MigrationStatus_IN_PROGRESS = MigrationStatus' "IN_PROGRESS"

pattern MigrationStatus_NOT_STARTED :: MigrationStatus
pattern MigrationStatus_NOT_STARTED = MigrationStatus' "NOT_STARTED"

{-# COMPLETE
  MigrationStatus_COMPLETED,
  MigrationStatus_FAILED,
  MigrationStatus_IN_PROGRESS,
  MigrationStatus_NOT_STARTED,
  MigrationStatus'
  #-}
