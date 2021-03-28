{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.VideoJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.VideoJobStatus
  ( VideoJobStatus
    ( VideoJobStatus'
    , VideoJobStatusInProgress
    , VideoJobStatusSucceeded
    , VideoJobStatusFailed
    , fromVideoJobStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype VideoJobStatus = VideoJobStatus'{fromVideoJobStatus ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern VideoJobStatusInProgress :: VideoJobStatus
pattern VideoJobStatusInProgress = VideoJobStatus' "IN_PROGRESS"

pattern VideoJobStatusSucceeded :: VideoJobStatus
pattern VideoJobStatusSucceeded = VideoJobStatus' "SUCCEEDED"

pattern VideoJobStatusFailed :: VideoJobStatus
pattern VideoJobStatusFailed = VideoJobStatus' "FAILED"

{-# COMPLETE 
  VideoJobStatusInProgress,

  VideoJobStatusSucceeded,

  VideoJobStatusFailed,
  VideoJobStatus'
  #-}
