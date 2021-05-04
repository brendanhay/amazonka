{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EventSeverity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EventSeverity
  ( EventSeverity
      ( ..,
        EventSeverity_DEBUG,
        EventSeverity_ERROR,
        EventSeverity_FATAL,
        EventSeverity_INFO,
        EventSeverity_TRACE,
        EventSeverity_WARN
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype EventSeverity = EventSeverity'
  { fromEventSeverity ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern EventSeverity_DEBUG :: EventSeverity
pattern EventSeverity_DEBUG = EventSeverity' "DEBUG"

pattern EventSeverity_ERROR :: EventSeverity
pattern EventSeverity_ERROR = EventSeverity' "ERROR"

pattern EventSeverity_FATAL :: EventSeverity
pattern EventSeverity_FATAL = EventSeverity' "FATAL"

pattern EventSeverity_INFO :: EventSeverity
pattern EventSeverity_INFO = EventSeverity' "INFO"

pattern EventSeverity_TRACE :: EventSeverity
pattern EventSeverity_TRACE = EventSeverity' "TRACE"

pattern EventSeverity_WARN :: EventSeverity
pattern EventSeverity_WARN = EventSeverity' "WARN"

{-# COMPLETE
  EventSeverity_DEBUG,
  EventSeverity_ERROR,
  EventSeverity_FATAL,
  EventSeverity_INFO,
  EventSeverity_TRACE,
  EventSeverity_WARN,
  EventSeverity'
  #-}
