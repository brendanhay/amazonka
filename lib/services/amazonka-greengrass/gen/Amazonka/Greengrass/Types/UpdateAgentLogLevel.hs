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
-- Module      : Amazonka.Greengrass.Types.UpdateAgentLogLevel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.UpdateAgentLogLevel
  ( UpdateAgentLogLevel
      ( ..,
        UpdateAgentLogLevel_DEBUG,
        UpdateAgentLogLevel_ERROR,
        UpdateAgentLogLevel_FATAL,
        UpdateAgentLogLevel_INFO,
        UpdateAgentLogLevel_NONE,
        UpdateAgentLogLevel_TRACE,
        UpdateAgentLogLevel_VERBOSE,
        UpdateAgentLogLevel_WARN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The minimum level of log statements that should be logged by the OTA
-- Agent during an update.
newtype UpdateAgentLogLevel = UpdateAgentLogLevel'
  { fromUpdateAgentLogLevel ::
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

pattern UpdateAgentLogLevel_DEBUG :: UpdateAgentLogLevel
pattern UpdateAgentLogLevel_DEBUG = UpdateAgentLogLevel' "DEBUG"

pattern UpdateAgentLogLevel_ERROR :: UpdateAgentLogLevel
pattern UpdateAgentLogLevel_ERROR = UpdateAgentLogLevel' "ERROR"

pattern UpdateAgentLogLevel_FATAL :: UpdateAgentLogLevel
pattern UpdateAgentLogLevel_FATAL = UpdateAgentLogLevel' "FATAL"

pattern UpdateAgentLogLevel_INFO :: UpdateAgentLogLevel
pattern UpdateAgentLogLevel_INFO = UpdateAgentLogLevel' "INFO"

pattern UpdateAgentLogLevel_NONE :: UpdateAgentLogLevel
pattern UpdateAgentLogLevel_NONE = UpdateAgentLogLevel' "NONE"

pattern UpdateAgentLogLevel_TRACE :: UpdateAgentLogLevel
pattern UpdateAgentLogLevel_TRACE = UpdateAgentLogLevel' "TRACE"

pattern UpdateAgentLogLevel_VERBOSE :: UpdateAgentLogLevel
pattern UpdateAgentLogLevel_VERBOSE = UpdateAgentLogLevel' "VERBOSE"

pattern UpdateAgentLogLevel_WARN :: UpdateAgentLogLevel
pattern UpdateAgentLogLevel_WARN = UpdateAgentLogLevel' "WARN"

{-# COMPLETE
  UpdateAgentLogLevel_DEBUG,
  UpdateAgentLogLevel_ERROR,
  UpdateAgentLogLevel_FATAL,
  UpdateAgentLogLevel_INFO,
  UpdateAgentLogLevel_NONE,
  UpdateAgentLogLevel_TRACE,
  UpdateAgentLogLevel_VERBOSE,
  UpdateAgentLogLevel_WARN,
  UpdateAgentLogLevel'
  #-}
