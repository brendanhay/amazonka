{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.LogLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.LogLevel
  ( LogLevel
      ( LogLevel',
        LogLevelError,
        LogLevelWarning,
        LogLevelInfo,
        LogLevelDebug,
        LogLevelDisabled,
        fromLogLevel
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The log level the user wants for their channel.
newtype LogLevel = LogLevel' {fromLogLevel :: Core.Text}
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

pattern LogLevelError :: LogLevel
pattern LogLevelError = LogLevel' "ERROR"

pattern LogLevelWarning :: LogLevel
pattern LogLevelWarning = LogLevel' "WARNING"

pattern LogLevelInfo :: LogLevel
pattern LogLevelInfo = LogLevel' "INFO"

pattern LogLevelDebug :: LogLevel
pattern LogLevelDebug = LogLevel' "DEBUG"

pattern LogLevelDisabled :: LogLevel
pattern LogLevelDisabled = LogLevel' "DISABLED"

{-# COMPLETE
  LogLevelError,
  LogLevelWarning,
  LogLevelInfo,
  LogLevelDebug,
  LogLevelDisabled,
  LogLevel'
  #-}
