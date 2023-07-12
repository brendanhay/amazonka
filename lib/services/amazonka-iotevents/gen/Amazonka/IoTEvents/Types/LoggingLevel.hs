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
-- Module      : Amazonka.IoTEvents.Types.LoggingLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.LoggingLevel
  ( LoggingLevel
      ( ..,
        LoggingLevel_DEBUG,
        LoggingLevel_ERROR,
        LoggingLevel_INFO
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LoggingLevel = LoggingLevel'
  { fromLoggingLevel ::
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

pattern LoggingLevel_DEBUG :: LoggingLevel
pattern LoggingLevel_DEBUG = LoggingLevel' "DEBUG"

pattern LoggingLevel_ERROR :: LoggingLevel
pattern LoggingLevel_ERROR = LoggingLevel' "ERROR"

pattern LoggingLevel_INFO :: LoggingLevel
pattern LoggingLevel_INFO = LoggingLevel' "INFO"

{-# COMPLETE
  LoggingLevel_DEBUG,
  LoggingLevel_ERROR,
  LoggingLevel_INFO,
  LoggingLevel'
  #-}
