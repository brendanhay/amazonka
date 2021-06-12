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
-- Module      : Network.AWS.StepFunctions.Types.LogLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LogLevel
  ( LogLevel
      ( ..,
        LogLevel_ALL,
        LogLevel_ERROR,
        LogLevel_FATAL,
        LogLevel_OFF
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype LogLevel = LogLevel'
  { fromLogLevel ::
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

pattern LogLevel_ALL :: LogLevel
pattern LogLevel_ALL = LogLevel' "ALL"

pattern LogLevel_ERROR :: LogLevel
pattern LogLevel_ERROR = LogLevel' "ERROR"

pattern LogLevel_FATAL :: LogLevel
pattern LogLevel_FATAL = LogLevel' "FATAL"

pattern LogLevel_OFF :: LogLevel
pattern LogLevel_OFF = LogLevel' "OFF"

{-# COMPLETE
  LogLevel_ALL,
  LogLevel_ERROR,
  LogLevel_FATAL,
  LogLevel_OFF,
  LogLevel'
  #-}
