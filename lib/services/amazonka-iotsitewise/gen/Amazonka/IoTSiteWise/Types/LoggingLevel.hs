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
-- Module      : Amazonka.IoTSiteWise.Types.LoggingLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.LoggingLevel
  ( LoggingLevel
      ( ..,
        LoggingLevel_ERROR,
        LoggingLevel_INFO,
        LoggingLevel_OFF
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

pattern LoggingLevel_ERROR :: LoggingLevel
pattern LoggingLevel_ERROR = LoggingLevel' "ERROR"

pattern LoggingLevel_INFO :: LoggingLevel
pattern LoggingLevel_INFO = LoggingLevel' "INFO"

pattern LoggingLevel_OFF :: LoggingLevel
pattern LoggingLevel_OFF = LoggingLevel' "OFF"

{-# COMPLETE
  LoggingLevel_ERROR,
  LoggingLevel_INFO,
  LoggingLevel_OFF,
  LoggingLevel'
  #-}
