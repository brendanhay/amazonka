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
-- Module      : Amazonka.Athena.Types.QueryExecutionState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.QueryExecutionState
  ( QueryExecutionState
      ( ..,
        QueryExecutionState_CANCELLED,
        QueryExecutionState_FAILED,
        QueryExecutionState_QUEUED,
        QueryExecutionState_RUNNING,
        QueryExecutionState_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype QueryExecutionState = QueryExecutionState'
  { fromQueryExecutionState ::
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

pattern QueryExecutionState_CANCELLED :: QueryExecutionState
pattern QueryExecutionState_CANCELLED = QueryExecutionState' "CANCELLED"

pattern QueryExecutionState_FAILED :: QueryExecutionState
pattern QueryExecutionState_FAILED = QueryExecutionState' "FAILED"

pattern QueryExecutionState_QUEUED :: QueryExecutionState
pattern QueryExecutionState_QUEUED = QueryExecutionState' "QUEUED"

pattern QueryExecutionState_RUNNING :: QueryExecutionState
pattern QueryExecutionState_RUNNING = QueryExecutionState' "RUNNING"

pattern QueryExecutionState_SUCCEEDED :: QueryExecutionState
pattern QueryExecutionState_SUCCEEDED = QueryExecutionState' "SUCCEEDED"

{-# COMPLETE
  QueryExecutionState_CANCELLED,
  QueryExecutionState_FAILED,
  QueryExecutionState_QUEUED,
  QueryExecutionState_RUNNING,
  QueryExecutionState_SUCCEEDED,
  QueryExecutionState'
  #-}
