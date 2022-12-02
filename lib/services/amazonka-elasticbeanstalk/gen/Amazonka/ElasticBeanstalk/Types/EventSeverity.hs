{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticBeanstalk.Types.EventSeverity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.EventSeverity
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EventSeverity = EventSeverity'
  { fromEventSeverity ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
