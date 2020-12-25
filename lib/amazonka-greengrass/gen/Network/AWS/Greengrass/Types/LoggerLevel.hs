{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.LoggerLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.LoggerLevel
  ( LoggerLevel
      ( LoggerLevel',
        LoggerLevelDebug,
        LoggerLevelInfo,
        LoggerLevelWarn,
        LoggerLevelError,
        LoggerLevelFatal,
        fromLoggerLevel
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype LoggerLevel = LoggerLevel' {fromLoggerLevel :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern LoggerLevelDebug :: LoggerLevel
pattern LoggerLevelDebug = LoggerLevel' "DEBUG"

pattern LoggerLevelInfo :: LoggerLevel
pattern LoggerLevelInfo = LoggerLevel' "INFO"

pattern LoggerLevelWarn :: LoggerLevel
pattern LoggerLevelWarn = LoggerLevel' "WARN"

pattern LoggerLevelError :: LoggerLevel
pattern LoggerLevelError = LoggerLevel' "ERROR"

pattern LoggerLevelFatal :: LoggerLevel
pattern LoggerLevelFatal = LoggerLevel' "FATAL"

{-# COMPLETE
  LoggerLevelDebug,
  LoggerLevelInfo,
  LoggerLevelWarn,
  LoggerLevelError,
  LoggerLevelFatal,
  LoggerLevel'
  #-}
