{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.RunningMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.RunningMode
  ( RunningMode
    ( RunningMode'
    , RunningModeAutoStop
    , RunningModeAlwaysOn
    , fromRunningMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype RunningMode = RunningMode'{fromRunningMode :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern RunningModeAutoStop :: RunningMode
pattern RunningModeAutoStop = RunningMode' "AUTO_STOP"

pattern RunningModeAlwaysOn :: RunningMode
pattern RunningModeAlwaysOn = RunningMode' "ALWAYS_ON"

{-# COMPLETE 
  RunningModeAutoStop,

  RunningModeAlwaysOn,
  RunningMode'
  #-}
