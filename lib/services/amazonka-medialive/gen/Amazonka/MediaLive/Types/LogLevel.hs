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
-- Module      : Amazonka.MediaLive.Types.LogLevel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.LogLevel
  ( LogLevel
      ( ..,
        LogLevel_DEBUG,
        LogLevel_DISABLED,
        LogLevel_ERROR,
        LogLevel_INFO,
        LogLevel_WARNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The log level the user wants for their channel.
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

pattern LogLevel_DEBUG :: LogLevel
pattern LogLevel_DEBUG = LogLevel' "DEBUG"

pattern LogLevel_DISABLED :: LogLevel
pattern LogLevel_DISABLED = LogLevel' "DISABLED"

pattern LogLevel_ERROR :: LogLevel
pattern LogLevel_ERROR = LogLevel' "ERROR"

pattern LogLevel_INFO :: LogLevel
pattern LogLevel_INFO = LogLevel' "INFO"

pattern LogLevel_WARNING :: LogLevel
pattern LogLevel_WARNING = LogLevel' "WARNING"

{-# COMPLETE
  LogLevel_DEBUG,
  LogLevel_DISABLED,
  LogLevel_ERROR,
  LogLevel_INFO,
  LogLevel_WARNING,
  LogLevel'
  #-}
