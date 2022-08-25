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
-- Module      : Amazonka.ECS.Types.InstanceHealthCheckState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.InstanceHealthCheckState
  ( InstanceHealthCheckState
      ( ..,
        InstanceHealthCheckState_IMPAIRED,
        InstanceHealthCheckState_INITIALIZING,
        InstanceHealthCheckState_INSUFFICIENT_DATA,
        InstanceHealthCheckState_OK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype InstanceHealthCheckState = InstanceHealthCheckState'
  { fromInstanceHealthCheckState ::
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

pattern InstanceHealthCheckState_IMPAIRED :: InstanceHealthCheckState
pattern InstanceHealthCheckState_IMPAIRED = InstanceHealthCheckState' "IMPAIRED"

pattern InstanceHealthCheckState_INITIALIZING :: InstanceHealthCheckState
pattern InstanceHealthCheckState_INITIALIZING = InstanceHealthCheckState' "INITIALIZING"

pattern InstanceHealthCheckState_INSUFFICIENT_DATA :: InstanceHealthCheckState
pattern InstanceHealthCheckState_INSUFFICIENT_DATA = InstanceHealthCheckState' "INSUFFICIENT_DATA"

pattern InstanceHealthCheckState_OK :: InstanceHealthCheckState
pattern InstanceHealthCheckState_OK = InstanceHealthCheckState' "OK"

{-# COMPLETE
  InstanceHealthCheckState_IMPAIRED,
  InstanceHealthCheckState_INITIALIZING,
  InstanceHealthCheckState_INSUFFICIENT_DATA,
  InstanceHealthCheckState_OK,
  InstanceHealthCheckState'
  #-}
