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
-- Module      : Amazonka.IVSChat.Types.LoggingConfigurationState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSChat.Types.LoggingConfigurationState
  ( LoggingConfigurationState
      ( ..,
        LoggingConfigurationState_ACTIVE,
        LoggingConfigurationState_CREATE_FAILED,
        LoggingConfigurationState_CREATING,
        LoggingConfigurationState_DELETE_FAILED,
        LoggingConfigurationState_DELETING,
        LoggingConfigurationState_UPDATE_FAILED,
        LoggingConfigurationState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LoggingConfigurationState = LoggingConfigurationState'
  { fromLoggingConfigurationState ::
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

pattern LoggingConfigurationState_ACTIVE :: LoggingConfigurationState
pattern LoggingConfigurationState_ACTIVE = LoggingConfigurationState' "ACTIVE"

pattern LoggingConfigurationState_CREATE_FAILED :: LoggingConfigurationState
pattern LoggingConfigurationState_CREATE_FAILED = LoggingConfigurationState' "CREATE_FAILED"

pattern LoggingConfigurationState_CREATING :: LoggingConfigurationState
pattern LoggingConfigurationState_CREATING = LoggingConfigurationState' "CREATING"

pattern LoggingConfigurationState_DELETE_FAILED :: LoggingConfigurationState
pattern LoggingConfigurationState_DELETE_FAILED = LoggingConfigurationState' "DELETE_FAILED"

pattern LoggingConfigurationState_DELETING :: LoggingConfigurationState
pattern LoggingConfigurationState_DELETING = LoggingConfigurationState' "DELETING"

pattern LoggingConfigurationState_UPDATE_FAILED :: LoggingConfigurationState
pattern LoggingConfigurationState_UPDATE_FAILED = LoggingConfigurationState' "UPDATE_FAILED"

pattern LoggingConfigurationState_UPDATING :: LoggingConfigurationState
pattern LoggingConfigurationState_UPDATING = LoggingConfigurationState' "UPDATING"

{-# COMPLETE
  LoggingConfigurationState_ACTIVE,
  LoggingConfigurationState_CREATE_FAILED,
  LoggingConfigurationState_CREATING,
  LoggingConfigurationState_DELETE_FAILED,
  LoggingConfigurationState_DELETING,
  LoggingConfigurationState_UPDATE_FAILED,
  LoggingConfigurationState_UPDATING,
  LoggingConfigurationState'
  #-}
