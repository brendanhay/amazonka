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
-- Module      : Amazonka.StepFunctions.Types.LogLevel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.LogLevel
  ( LogLevel
      ( ..,
        LogLevel_ALL,
        LogLevel_ERROR,
        LogLevel_FATAL,
        LogLevel_OFF
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LogLevel = LogLevel'
  { fromLogLevel ::
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
