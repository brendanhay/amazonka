{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype QueryExecutionState = QueryExecutionState'
  { fromQueryExecutionState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
