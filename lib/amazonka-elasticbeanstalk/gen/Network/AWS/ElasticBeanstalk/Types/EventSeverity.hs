{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EventSeverity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EventSeverity
  ( EventSeverity
      ( EventSeverity',
        LevelTrace,
        LevelDebug,
        LevelInfo,
        LevelWarn,
        LevelError,
        LevelFatal
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EventSeverity = EventSeverity' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern LevelTrace :: EventSeverity
pattern LevelTrace = EventSeverity' "TRACE"

pattern LevelDebug :: EventSeverity
pattern LevelDebug = EventSeverity' "DEBUG"

pattern LevelInfo :: EventSeverity
pattern LevelInfo = EventSeverity' "INFO"

pattern LevelWarn :: EventSeverity
pattern LevelWarn = EventSeverity' "WARN"

pattern LevelError :: EventSeverity
pattern LevelError = EventSeverity' "ERROR"

pattern LevelFatal :: EventSeverity
pattern LevelFatal = EventSeverity' "FATAL"

{-# COMPLETE
  LevelTrace,
  LevelDebug,
  LevelInfo,
  LevelWarn,
  LevelError,
  LevelFatal,
  EventSeverity'
  #-}
