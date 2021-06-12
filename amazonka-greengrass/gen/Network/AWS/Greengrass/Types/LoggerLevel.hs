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
-- Module      : Network.AWS.Greengrass.Types.LoggerLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.LoggerLevel
  ( LoggerLevel
      ( ..,
        LoggerLevel_DEBUG,
        LoggerLevel_ERROR,
        LoggerLevel_FATAL,
        LoggerLevel_INFO,
        LoggerLevel_WARN
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype LoggerLevel = LoggerLevel'
  { fromLoggerLevel ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern LoggerLevel_DEBUG :: LoggerLevel
pattern LoggerLevel_DEBUG = LoggerLevel' "DEBUG"

pattern LoggerLevel_ERROR :: LoggerLevel
pattern LoggerLevel_ERROR = LoggerLevel' "ERROR"

pattern LoggerLevel_FATAL :: LoggerLevel
pattern LoggerLevel_FATAL = LoggerLevel' "FATAL"

pattern LoggerLevel_INFO :: LoggerLevel
pattern LoggerLevel_INFO = LoggerLevel' "INFO"

pattern LoggerLevel_WARN :: LoggerLevel
pattern LoggerLevel_WARN = LoggerLevel' "WARN"

{-# COMPLETE
  LoggerLevel_DEBUG,
  LoggerLevel_ERROR,
  LoggerLevel_FATAL,
  LoggerLevel_INFO,
  LoggerLevel_WARN,
  LoggerLevel'
  #-}
