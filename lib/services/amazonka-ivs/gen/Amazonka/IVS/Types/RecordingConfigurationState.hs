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
-- Module      : Amazonka.IVS.Types.RecordingConfigurationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVS.Types.RecordingConfigurationState
  ( RecordingConfigurationState
      ( ..,
        RecordingConfigurationState_ACTIVE,
        RecordingConfigurationState_CREATE_FAILED,
        RecordingConfigurationState_CREATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RecordingConfigurationState = RecordingConfigurationState'
  { fromRecordingConfigurationState ::
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

pattern RecordingConfigurationState_ACTIVE :: RecordingConfigurationState
pattern RecordingConfigurationState_ACTIVE = RecordingConfigurationState' "ACTIVE"

pattern RecordingConfigurationState_CREATE_FAILED :: RecordingConfigurationState
pattern RecordingConfigurationState_CREATE_FAILED = RecordingConfigurationState' "CREATE_FAILED"

pattern RecordingConfigurationState_CREATING :: RecordingConfigurationState
pattern RecordingConfigurationState_CREATING = RecordingConfigurationState' "CREATING"

{-# COMPLETE
  RecordingConfigurationState_ACTIVE,
  RecordingConfigurationState_CREATE_FAILED,
  RecordingConfigurationState_CREATING,
  RecordingConfigurationState'
  #-}
