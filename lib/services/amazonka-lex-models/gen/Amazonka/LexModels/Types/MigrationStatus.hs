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
-- Module      : Amazonka.LexModels.Types.MigrationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.MigrationStatus
  ( MigrationStatus
      ( ..,
        MigrationStatus_COMPLETED,
        MigrationStatus_FAILED,
        MigrationStatus_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype MigrationStatus = MigrationStatus'
  { fromMigrationStatus ::
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

pattern MigrationStatus_COMPLETED :: MigrationStatus
pattern MigrationStatus_COMPLETED = MigrationStatus' "COMPLETED"

pattern MigrationStatus_FAILED :: MigrationStatus
pattern MigrationStatus_FAILED = MigrationStatus' "FAILED"

pattern MigrationStatus_IN_PROGRESS :: MigrationStatus
pattern MigrationStatus_IN_PROGRESS = MigrationStatus' "IN_PROGRESS"

{-# COMPLETE
  MigrationStatus_COMPLETED,
  MigrationStatus_FAILED,
  MigrationStatus_IN_PROGRESS,
  MigrationStatus'
  #-}
