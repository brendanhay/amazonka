{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ContinuousExportStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.ContinuousExportStatus
  ( ContinuousExportStatus
    ( ContinuousExportStatus'
    , ContinuousExportStatusStartInProgress
    , ContinuousExportStatusStartFailed
    , ContinuousExportStatusActive
    , ContinuousExportStatusError
    , ContinuousExportStatusStopInProgress
    , ContinuousExportStatusStopFailed
    , ContinuousExportStatusInactive
    , fromContinuousExportStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ContinuousExportStatus = ContinuousExportStatus'{fromContinuousExportStatus
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern ContinuousExportStatusStartInProgress :: ContinuousExportStatus
pattern ContinuousExportStatusStartInProgress = ContinuousExportStatus' "START_IN_PROGRESS"

pattern ContinuousExportStatusStartFailed :: ContinuousExportStatus
pattern ContinuousExportStatusStartFailed = ContinuousExportStatus' "START_FAILED"

pattern ContinuousExportStatusActive :: ContinuousExportStatus
pattern ContinuousExportStatusActive = ContinuousExportStatus' "ACTIVE"

pattern ContinuousExportStatusError :: ContinuousExportStatus
pattern ContinuousExportStatusError = ContinuousExportStatus' "ERROR"

pattern ContinuousExportStatusStopInProgress :: ContinuousExportStatus
pattern ContinuousExportStatusStopInProgress = ContinuousExportStatus' "STOP_IN_PROGRESS"

pattern ContinuousExportStatusStopFailed :: ContinuousExportStatus
pattern ContinuousExportStatusStopFailed = ContinuousExportStatus' "STOP_FAILED"

pattern ContinuousExportStatusInactive :: ContinuousExportStatus
pattern ContinuousExportStatusInactive = ContinuousExportStatus' "INACTIVE"

{-# COMPLETE 
  ContinuousExportStatusStartInProgress,

  ContinuousExportStatusStartFailed,

  ContinuousExportStatusActive,

  ContinuousExportStatusError,

  ContinuousExportStatusStopInProgress,

  ContinuousExportStatusStopFailed,

  ContinuousExportStatusInactive,
  ContinuousExportStatus'
  #-}
