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
-- Module      : Amazonka.IoTWireless.Types.LogLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LogLevel
  ( LogLevel
      ( ..,
        LogLevel_DISABLED,
        LogLevel_ERROR,
        LogLevel_INFO
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The log level for a log message. The log levels can be disabled, or set
-- to @ERROR@ to display less verbose logs containing only error
-- information, or to @INFO@ for more detailed logs.
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

pattern LogLevel_DISABLED :: LogLevel
pattern LogLevel_DISABLED = LogLevel' "DISABLED"

pattern LogLevel_ERROR :: LogLevel
pattern LogLevel_ERROR = LogLevel' "ERROR"

pattern LogLevel_INFO :: LogLevel
pattern LogLevel_INFO = LogLevel' "INFO"

{-# COMPLETE
  LogLevel_DISABLED,
  LogLevel_ERROR,
  LogLevel_INFO,
  LogLevel'
  #-}
