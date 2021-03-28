{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.BulkPublishStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoSync.Types.BulkPublishStatus
  ( BulkPublishStatus
    ( BulkPublishStatus'
    , BulkPublishStatusNotStarted
    , BulkPublishStatusInProgress
    , BulkPublishStatusFailed
    , BulkPublishStatusSucceeded
    , fromBulkPublishStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype BulkPublishStatus = BulkPublishStatus'{fromBulkPublishStatus
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern BulkPublishStatusNotStarted :: BulkPublishStatus
pattern BulkPublishStatusNotStarted = BulkPublishStatus' "NOT_STARTED"

pattern BulkPublishStatusInProgress :: BulkPublishStatus
pattern BulkPublishStatusInProgress = BulkPublishStatus' "IN_PROGRESS"

pattern BulkPublishStatusFailed :: BulkPublishStatus
pattern BulkPublishStatusFailed = BulkPublishStatus' "FAILED"

pattern BulkPublishStatusSucceeded :: BulkPublishStatus
pattern BulkPublishStatusSucceeded = BulkPublishStatus' "SUCCEEDED"

{-# COMPLETE 
  BulkPublishStatusNotStarted,

  BulkPublishStatusInProgress,

  BulkPublishStatusFailed,

  BulkPublishStatusSucceeded,
  BulkPublishStatus'
  #-}
