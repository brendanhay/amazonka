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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceHealthCheckState = InstanceHealthCheckState'
  { fromInstanceHealthCheckState ::
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
