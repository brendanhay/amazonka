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
-- Module      : Amazonka.SimSpaceWeaver.Types.LifecycleManagementStrategy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.LifecycleManagementStrategy
  ( LifecycleManagementStrategy
      ( ..,
        LifecycleManagementStrategy_ByRequest,
        LifecycleManagementStrategy_BySpatialSubdivision,
        LifecycleManagementStrategy_PerWorker,
        LifecycleManagementStrategy_Unknown
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LifecycleManagementStrategy = LifecycleManagementStrategy'
  { fromLifecycleManagementStrategy ::
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

pattern LifecycleManagementStrategy_ByRequest :: LifecycleManagementStrategy
pattern LifecycleManagementStrategy_ByRequest = LifecycleManagementStrategy' "ByRequest"

pattern LifecycleManagementStrategy_BySpatialSubdivision :: LifecycleManagementStrategy
pattern LifecycleManagementStrategy_BySpatialSubdivision = LifecycleManagementStrategy' "BySpatialSubdivision"

pattern LifecycleManagementStrategy_PerWorker :: LifecycleManagementStrategy
pattern LifecycleManagementStrategy_PerWorker = LifecycleManagementStrategy' "PerWorker"

pattern LifecycleManagementStrategy_Unknown :: LifecycleManagementStrategy
pattern LifecycleManagementStrategy_Unknown = LifecycleManagementStrategy' "Unknown"

{-# COMPLETE
  LifecycleManagementStrategy_ByRequest,
  LifecycleManagementStrategy_BySpatialSubdivision,
  LifecycleManagementStrategy_PerWorker,
  LifecycleManagementStrategy_Unknown,
  LifecycleManagementStrategy'
  #-}
