{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CommandPluginStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.CommandPluginStatus
  ( CommandPluginStatus
    ( CommandPluginStatus'
    , CommandPluginStatusPending
    , CommandPluginStatusInProgress
    , CommandPluginStatusSuccess
    , CommandPluginStatusTimedOut
    , CommandPluginStatusCancelled
    , CommandPluginStatusFailed
    , fromCommandPluginStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype CommandPluginStatus = CommandPluginStatus'{fromCommandPluginStatus
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern CommandPluginStatusPending :: CommandPluginStatus
pattern CommandPluginStatusPending = CommandPluginStatus' "Pending"

pattern CommandPluginStatusInProgress :: CommandPluginStatus
pattern CommandPluginStatusInProgress = CommandPluginStatus' "InProgress"

pattern CommandPluginStatusSuccess :: CommandPluginStatus
pattern CommandPluginStatusSuccess = CommandPluginStatus' "Success"

pattern CommandPluginStatusTimedOut :: CommandPluginStatus
pattern CommandPluginStatusTimedOut = CommandPluginStatus' "TimedOut"

pattern CommandPluginStatusCancelled :: CommandPluginStatus
pattern CommandPluginStatusCancelled = CommandPluginStatus' "Cancelled"

pattern CommandPluginStatusFailed :: CommandPluginStatus
pattern CommandPluginStatusFailed = CommandPluginStatus' "Failed"

{-# COMPLETE 
  CommandPluginStatusPending,

  CommandPluginStatusInProgress,

  CommandPluginStatusSuccess,

  CommandPluginStatusTimedOut,

  CommandPluginStatusCancelled,

  CommandPluginStatusFailed,
  CommandPluginStatus'
  #-}
