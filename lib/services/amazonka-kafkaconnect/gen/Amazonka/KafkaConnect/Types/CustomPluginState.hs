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
-- Module      : Amazonka.KafkaConnect.Types.CustomPluginState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.CustomPluginState
  ( CustomPluginState
      ( ..,
        CustomPluginState_ACTIVE,
        CustomPluginState_CREATE_FAILED,
        CustomPluginState_CREATING,
        CustomPluginState_DELETING,
        CustomPluginState_UPDATE_FAILED,
        CustomPluginState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CustomPluginState = CustomPluginState'
  { fromCustomPluginState ::
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

pattern CustomPluginState_ACTIVE :: CustomPluginState
pattern CustomPluginState_ACTIVE = CustomPluginState' "ACTIVE"

pattern CustomPluginState_CREATE_FAILED :: CustomPluginState
pattern CustomPluginState_CREATE_FAILED = CustomPluginState' "CREATE_FAILED"

pattern CustomPluginState_CREATING :: CustomPluginState
pattern CustomPluginState_CREATING = CustomPluginState' "CREATING"

pattern CustomPluginState_DELETING :: CustomPluginState
pattern CustomPluginState_DELETING = CustomPluginState' "DELETING"

pattern CustomPluginState_UPDATE_FAILED :: CustomPluginState
pattern CustomPluginState_UPDATE_FAILED = CustomPluginState' "UPDATE_FAILED"

pattern CustomPluginState_UPDATING :: CustomPluginState
pattern CustomPluginState_UPDATING = CustomPluginState' "UPDATING"

{-# COMPLETE
  CustomPluginState_ACTIVE,
  CustomPluginState_CREATE_FAILED,
  CustomPluginState_CREATING,
  CustomPluginState_DELETING,
  CustomPluginState_UPDATE_FAILED,
  CustomPluginState_UPDATING,
  CustomPluginState'
  #-}
