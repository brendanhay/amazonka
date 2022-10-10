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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

newtype QueryExecutionState = QueryExecutionState'
  { fromQueryExecutionState ::
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
