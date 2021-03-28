{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EventSeverity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.EventSeverity
  ( EventSeverity
    ( EventSeverity'
    , EventSeverityLevelTrace
    , EventSeverityLevelDebug
    , EventSeverityLevelInfo
    , EventSeverityLevelWarn
    , EventSeverityLevelError
    , EventSeverityLevelFatal
    , fromEventSeverity
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype EventSeverity = EventSeverity'{fromEventSeverity ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern EventSeverityLevelTrace :: EventSeverity
pattern EventSeverityLevelTrace = EventSeverity' "TRACE"

pattern EventSeverityLevelDebug :: EventSeverity
pattern EventSeverityLevelDebug = EventSeverity' "DEBUG"

pattern EventSeverityLevelInfo :: EventSeverity
pattern EventSeverityLevelInfo = EventSeverity' "INFO"

pattern EventSeverityLevelWarn :: EventSeverity
pattern EventSeverityLevelWarn = EventSeverity' "WARN"

pattern EventSeverityLevelError :: EventSeverity
pattern EventSeverityLevelError = EventSeverity' "ERROR"

pattern EventSeverityLevelFatal :: EventSeverity
pattern EventSeverityLevelFatal = EventSeverity' "FATAL"

{-# COMPLETE 
  EventSeverityLevelTrace,

  EventSeverityLevelDebug,

  EventSeverityLevelInfo,

  EventSeverityLevelWarn,

  EventSeverityLevelError,

  EventSeverityLevelFatal,
  EventSeverity'
  #-}
