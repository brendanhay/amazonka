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
-- Module      : Amazonka.Greengrass.Types.LoggerLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.LoggerLevel
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LoggerLevel = LoggerLevel'
  { fromLoggerLevel ::
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
