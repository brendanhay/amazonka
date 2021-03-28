{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.UpdateAgentLogLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.UpdateAgentLogLevel
  ( UpdateAgentLogLevel
    ( UpdateAgentLogLevel'
    , UpdateAgentLogLevelNone
    , UpdateAgentLogLevelTrace
    , UpdateAgentLogLevelDebug
    , UpdateAgentLogLevelVerbose
    , UpdateAgentLogLevelInfo
    , UpdateAgentLogLevelWarn
    , UpdateAgentLogLevelError
    , UpdateAgentLogLevelFatal
    , fromUpdateAgentLogLevel
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The minimum level of log statements that should be logged by the OTA Agent during an update.
newtype UpdateAgentLogLevel = UpdateAgentLogLevel'{fromUpdateAgentLogLevel
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern UpdateAgentLogLevelNone :: UpdateAgentLogLevel
pattern UpdateAgentLogLevelNone = UpdateAgentLogLevel' "NONE"

pattern UpdateAgentLogLevelTrace :: UpdateAgentLogLevel
pattern UpdateAgentLogLevelTrace = UpdateAgentLogLevel' "TRACE"

pattern UpdateAgentLogLevelDebug :: UpdateAgentLogLevel
pattern UpdateAgentLogLevelDebug = UpdateAgentLogLevel' "DEBUG"

pattern UpdateAgentLogLevelVerbose :: UpdateAgentLogLevel
pattern UpdateAgentLogLevelVerbose = UpdateAgentLogLevel' "VERBOSE"

pattern UpdateAgentLogLevelInfo :: UpdateAgentLogLevel
pattern UpdateAgentLogLevelInfo = UpdateAgentLogLevel' "INFO"

pattern UpdateAgentLogLevelWarn :: UpdateAgentLogLevel
pattern UpdateAgentLogLevelWarn = UpdateAgentLogLevel' "WARN"

pattern UpdateAgentLogLevelError :: UpdateAgentLogLevel
pattern UpdateAgentLogLevelError = UpdateAgentLogLevel' "ERROR"

pattern UpdateAgentLogLevelFatal :: UpdateAgentLogLevel
pattern UpdateAgentLogLevelFatal = UpdateAgentLogLevel' "FATAL"

{-# COMPLETE 
  UpdateAgentLogLevelNone,

  UpdateAgentLogLevelTrace,

  UpdateAgentLogLevelDebug,

  UpdateAgentLogLevelVerbose,

  UpdateAgentLogLevelInfo,

  UpdateAgentLogLevelWarn,

  UpdateAgentLogLevelError,

  UpdateAgentLogLevelFatal,
  UpdateAgentLogLevel'
  #-}
