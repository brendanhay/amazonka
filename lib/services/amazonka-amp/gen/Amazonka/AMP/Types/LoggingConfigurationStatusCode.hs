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
-- Module      : Amazonka.AMP.Types.LoggingConfigurationStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AMP.Types.LoggingConfigurationStatusCode
  ( LoggingConfigurationStatusCode
      ( ..,
        LoggingConfigurationStatusCode_ACTIVE,
        LoggingConfigurationStatusCode_CREATING,
        LoggingConfigurationStatusCode_CREATION_FAILED,
        LoggingConfigurationStatusCode_DELETING,
        LoggingConfigurationStatusCode_UPDATE_FAILED,
        LoggingConfigurationStatusCode_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | State of a logging configuration.
newtype LoggingConfigurationStatusCode = LoggingConfigurationStatusCode'
  { fromLoggingConfigurationStatusCode ::
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

pattern LoggingConfigurationStatusCode_ACTIVE :: LoggingConfigurationStatusCode
pattern LoggingConfigurationStatusCode_ACTIVE = LoggingConfigurationStatusCode' "ACTIVE"

pattern LoggingConfigurationStatusCode_CREATING :: LoggingConfigurationStatusCode
pattern LoggingConfigurationStatusCode_CREATING = LoggingConfigurationStatusCode' "CREATING"

pattern LoggingConfigurationStatusCode_CREATION_FAILED :: LoggingConfigurationStatusCode
pattern LoggingConfigurationStatusCode_CREATION_FAILED = LoggingConfigurationStatusCode' "CREATION_FAILED"

pattern LoggingConfigurationStatusCode_DELETING :: LoggingConfigurationStatusCode
pattern LoggingConfigurationStatusCode_DELETING = LoggingConfigurationStatusCode' "DELETING"

pattern LoggingConfigurationStatusCode_UPDATE_FAILED :: LoggingConfigurationStatusCode
pattern LoggingConfigurationStatusCode_UPDATE_FAILED = LoggingConfigurationStatusCode' "UPDATE_FAILED"

pattern LoggingConfigurationStatusCode_UPDATING :: LoggingConfigurationStatusCode
pattern LoggingConfigurationStatusCode_UPDATING = LoggingConfigurationStatusCode' "UPDATING"

{-# COMPLETE
  LoggingConfigurationStatusCode_ACTIVE,
  LoggingConfigurationStatusCode_CREATING,
  LoggingConfigurationStatusCode_CREATION_FAILED,
  LoggingConfigurationStatusCode_DELETING,
  LoggingConfigurationStatusCode_UPDATE_FAILED,
  LoggingConfigurationStatusCode_UPDATING,
  LoggingConfigurationStatusCode'
  #-}
