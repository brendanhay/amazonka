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
-- Module      : Amazonka.FSx.Types.DataRepositoryLifecycle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DataRepositoryLifecycle
  ( DataRepositoryLifecycle
      ( ..,
        DataRepositoryLifecycle_AVAILABLE,
        DataRepositoryLifecycle_CREATING,
        DataRepositoryLifecycle_DELETING,
        DataRepositoryLifecycle_FAILED,
        DataRepositoryLifecycle_MISCONFIGURED,
        DataRepositoryLifecycle_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataRepositoryLifecycle = DataRepositoryLifecycle'
  { fromDataRepositoryLifecycle ::
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

pattern DataRepositoryLifecycle_AVAILABLE :: DataRepositoryLifecycle
pattern DataRepositoryLifecycle_AVAILABLE = DataRepositoryLifecycle' "AVAILABLE"

pattern DataRepositoryLifecycle_CREATING :: DataRepositoryLifecycle
pattern DataRepositoryLifecycle_CREATING = DataRepositoryLifecycle' "CREATING"

pattern DataRepositoryLifecycle_DELETING :: DataRepositoryLifecycle
pattern DataRepositoryLifecycle_DELETING = DataRepositoryLifecycle' "DELETING"

pattern DataRepositoryLifecycle_FAILED :: DataRepositoryLifecycle
pattern DataRepositoryLifecycle_FAILED = DataRepositoryLifecycle' "FAILED"

pattern DataRepositoryLifecycle_MISCONFIGURED :: DataRepositoryLifecycle
pattern DataRepositoryLifecycle_MISCONFIGURED = DataRepositoryLifecycle' "MISCONFIGURED"

pattern DataRepositoryLifecycle_UPDATING :: DataRepositoryLifecycle
pattern DataRepositoryLifecycle_UPDATING = DataRepositoryLifecycle' "UPDATING"

{-# COMPLETE
  DataRepositoryLifecycle_AVAILABLE,
  DataRepositoryLifecycle_CREATING,
  DataRepositoryLifecycle_DELETING,
  DataRepositoryLifecycle_FAILED,
  DataRepositoryLifecycle_MISCONFIGURED,
  DataRepositoryLifecycle_UPDATING,
  DataRepositoryLifecycle'
  #-}
