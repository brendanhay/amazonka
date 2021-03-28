{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.SplunkS3BackupMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.SplunkS3BackupMode
  ( SplunkS3BackupMode
    ( SplunkS3BackupMode'
    , SplunkS3BackupModeFailedEventsOnly
    , SplunkS3BackupModeAllEvents
    , fromSplunkS3BackupMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype SplunkS3BackupMode = SplunkS3BackupMode'{fromSplunkS3BackupMode
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern SplunkS3BackupModeFailedEventsOnly :: SplunkS3BackupMode
pattern SplunkS3BackupModeFailedEventsOnly = SplunkS3BackupMode' "FailedEventsOnly"

pattern SplunkS3BackupModeAllEvents :: SplunkS3BackupMode
pattern SplunkS3BackupModeAllEvents = SplunkS3BackupMode' "AllEvents"

{-# COMPLETE 
  SplunkS3BackupModeFailedEventsOnly,

  SplunkS3BackupModeAllEvents,
  SplunkS3BackupMode'
  #-}
