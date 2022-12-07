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
-- Module      : Amazonka.ComputeOptimizer.Types.MigrationEffort
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.MigrationEffort
  ( MigrationEffort
      ( ..,
        MigrationEffort_High,
        MigrationEffort_Low,
        MigrationEffort_Medium,
        MigrationEffort_VeryLow
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MigrationEffort = MigrationEffort'
  { fromMigrationEffort ::
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

pattern MigrationEffort_High :: MigrationEffort
pattern MigrationEffort_High = MigrationEffort' "High"

pattern MigrationEffort_Low :: MigrationEffort
pattern MigrationEffort_Low = MigrationEffort' "Low"

pattern MigrationEffort_Medium :: MigrationEffort
pattern MigrationEffort_Medium = MigrationEffort' "Medium"

pattern MigrationEffort_VeryLow :: MigrationEffort
pattern MigrationEffort_VeryLow = MigrationEffort' "VeryLow"

{-# COMPLETE
  MigrationEffort_High,
  MigrationEffort_Low,
  MigrationEffort_Medium,
  MigrationEffort_VeryLow,
  MigrationEffort'
  #-}
