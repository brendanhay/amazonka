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
-- Module      : Network.AWS.Athena.Types.QueryExecutionState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.QueryExecutionState
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

import qualified Network.AWS.Core as Core

newtype QueryExecutionState = QueryExecutionState'
  { fromQueryExecutionState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
