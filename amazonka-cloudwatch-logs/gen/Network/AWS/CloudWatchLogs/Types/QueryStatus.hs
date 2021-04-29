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
-- Module      : Network.AWS.CloudWatchLogs.Types.QueryStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.QueryStatus
  ( QueryStatus
      ( ..,
        QueryStatus_Cancelled,
        QueryStatus_Complete,
        QueryStatus_Failed,
        QueryStatus_Running,
        QueryStatus_Scheduled
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype QueryStatus = QueryStatus'
  { fromQueryStatus ::
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

{-# COMPLETE
  QueryStatus_Cancelled,
  QueryStatus_Complete,
  QueryStatus_Failed,
  QueryStatus_Running,
  QueryStatus_Scheduled,
  QueryStatus'
  #-}
