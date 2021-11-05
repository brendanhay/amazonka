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
-- Module      : Network.AWS.KafkaConnect.Types.CustomPluginState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KafkaConnect.Types.CustomPluginState
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype CustomPluginState = CustomPluginState'
  { fromCustomPluginState ::
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
