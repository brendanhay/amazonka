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
-- Module      : Amazonka.CloudTrail.Types.QueryStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.QueryStatus
  ( QueryStatus
      ( ..,
        QueryStatus_CANCELLED,
        QueryStatus_FAILED,
        QueryStatus_FINISHED,
        QueryStatus_QUEUED,
        QueryStatus_RUNNING,
        QueryStatus_TIMED_OUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype QueryStatus = QueryStatus'
  { fromQueryStatus ::
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

pattern QueryStatus_CANCELLED :: QueryStatus
pattern QueryStatus_CANCELLED = QueryStatus' "CANCELLED"

pattern QueryStatus_FAILED :: QueryStatus
pattern QueryStatus_FAILED = QueryStatus' "FAILED"

pattern QueryStatus_FINISHED :: QueryStatus
pattern QueryStatus_FINISHED = QueryStatus' "FINISHED"

pattern QueryStatus_QUEUED :: QueryStatus
pattern QueryStatus_QUEUED = QueryStatus' "QUEUED"

pattern QueryStatus_RUNNING :: QueryStatus
pattern QueryStatus_RUNNING = QueryStatus' "RUNNING"

pattern QueryStatus_TIMED_OUT :: QueryStatus
pattern QueryStatus_TIMED_OUT = QueryStatus' "TIMED_OUT"

{-# COMPLETE
  QueryStatus_CANCELLED,
  QueryStatus_FAILED,
  QueryStatus_FINISHED,
  QueryStatus_QUEUED,
  QueryStatus_RUNNING,
  QueryStatus_TIMED_OUT,
  QueryStatus'
  #-}
