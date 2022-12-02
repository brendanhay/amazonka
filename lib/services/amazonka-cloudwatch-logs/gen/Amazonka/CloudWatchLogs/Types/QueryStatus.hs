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
-- Module      : Amazonka.CloudWatchLogs.Types.QueryStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.QueryStatus
  ( QueryStatus
      ( ..,
        QueryStatus_Cancelled,
        QueryStatus_Complete,
        QueryStatus_Failed,
        QueryStatus_Running,
        QueryStatus_Scheduled,
        QueryStatus_Timeout,
        QueryStatus_Unknown
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype QueryStatus = QueryStatus'
  { fromQueryStatus ::
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

pattern QueryStatus_Cancelled :: QueryStatus
pattern QueryStatus_Cancelled = QueryStatus' "Cancelled"

pattern QueryStatus_Complete :: QueryStatus
pattern QueryStatus_Complete = QueryStatus' "Complete"

pattern QueryStatus_Failed :: QueryStatus
pattern QueryStatus_Failed = QueryStatus' "Failed"

pattern QueryStatus_Running :: QueryStatus
pattern QueryStatus_Running = QueryStatus' "Running"

pattern QueryStatus_Scheduled :: QueryStatus
pattern QueryStatus_Scheduled = QueryStatus' "Scheduled"

pattern QueryStatus_Timeout :: QueryStatus
pattern QueryStatus_Timeout = QueryStatus' "Timeout"

pattern QueryStatus_Unknown :: QueryStatus
pattern QueryStatus_Unknown = QueryStatus' "Unknown"

{-# COMPLETE
  QueryStatus_Cancelled,
  QueryStatus_Complete,
  QueryStatus_Failed,
  QueryStatus_Running,
  QueryStatus_Scheduled,
  QueryStatus_Timeout,
  QueryStatus_Unknown,
  QueryStatus'
  #-}
