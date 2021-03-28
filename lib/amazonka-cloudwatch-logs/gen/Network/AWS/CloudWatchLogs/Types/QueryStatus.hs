{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.QueryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.QueryStatus
  ( QueryStatus
    ( QueryStatus'
    , QueryStatusScheduled
    , QueryStatusRunning
    , QueryStatusComplete
    , QueryStatusFailed
    , QueryStatusCancelled
    , fromQueryStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype QueryStatus = QueryStatus'{fromQueryStatus :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern QueryStatusScheduled :: QueryStatus
pattern QueryStatusScheduled = QueryStatus' "Scheduled"

pattern QueryStatusRunning :: QueryStatus
pattern QueryStatusRunning = QueryStatus' "Running"

pattern QueryStatusComplete :: QueryStatus
pattern QueryStatusComplete = QueryStatus' "Complete"

pattern QueryStatusFailed :: QueryStatus
pattern QueryStatusFailed = QueryStatus' "Failed"

pattern QueryStatusCancelled :: QueryStatus
pattern QueryStatusCancelled = QueryStatus' "Cancelled"

{-# COMPLETE 
  QueryStatusScheduled,

  QueryStatusRunning,

  QueryStatusComplete,

  QueryStatusFailed,

  QueryStatusCancelled,
  QueryStatus'
  #-}
