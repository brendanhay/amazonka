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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.LogLevel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.LogLevel
  ( LogLevel
      ( ..,
        LogLevel_DEBUG,
        LogLevel_ERROR,
        LogLevel_INFO,
        LogLevel_WARN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype LogLevel = LogLevel'
  { fromLogLevel ::
      Core.Text
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

pattern LogLevel_DEBUG :: LogLevel
pattern LogLevel_DEBUG = LogLevel' "DEBUG"

pattern LogLevel_ERROR :: LogLevel
pattern LogLevel_ERROR = LogLevel' "ERROR"

pattern LogLevel_INFO :: LogLevel
pattern LogLevel_INFO = LogLevel' "INFO"

pattern LogLevel_WARN :: LogLevel
pattern LogLevel_WARN = LogLevel' "WARN"

{-# COMPLETE
  LogLevel_DEBUG,
  LogLevel_ERROR,
  LogLevel_INFO,
  LogLevel_WARN,
  LogLevel'
  #-}
