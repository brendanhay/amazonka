{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LogLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StepFunctions.Types.LogLevel
  ( LogLevel
    ( LogLevel'
    , LogLevelAll
    , LogLevelError
    , LogLevelFatal
    , LogLevelOff
    , fromLogLevel
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype LogLevel = LogLevel'{fromLogLevel :: Core.Text}
                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                     Core.Generic)
                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                       Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                       Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                       Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern LogLevelAll :: LogLevel
pattern LogLevelAll = LogLevel' "ALL"

pattern LogLevelError :: LogLevel
pattern LogLevelError = LogLevel' "ERROR"

pattern LogLevelFatal :: LogLevel
pattern LogLevelFatal = LogLevel' "FATAL"

pattern LogLevelOff :: LogLevel
pattern LogLevelOff = LogLevel' "OFF"

{-# COMPLETE 
  LogLevelAll,

  LogLevelError,

  LogLevelFatal,

  LogLevelOff,
  LogLevel'
  #-}
