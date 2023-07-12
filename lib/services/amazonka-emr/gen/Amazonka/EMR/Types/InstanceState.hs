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
-- Module      : Amazonka.EMR.Types.InstanceState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceState
  ( InstanceState
      ( ..,
        InstanceState_AWAITING_FULFILLMENT,
        InstanceState_BOOTSTRAPPING,
        InstanceState_PROVISIONING,
        InstanceState_RUNNING,
        InstanceState_TERMINATED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceState = InstanceState'
  { fromInstanceState ::
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

pattern InstanceState_AWAITING_FULFILLMENT :: InstanceState
pattern InstanceState_AWAITING_FULFILLMENT = InstanceState' "AWAITING_FULFILLMENT"

pattern InstanceState_BOOTSTRAPPING :: InstanceState
pattern InstanceState_BOOTSTRAPPING = InstanceState' "BOOTSTRAPPING"

pattern InstanceState_PROVISIONING :: InstanceState
pattern InstanceState_PROVISIONING = InstanceState' "PROVISIONING"

pattern InstanceState_RUNNING :: InstanceState
pattern InstanceState_RUNNING = InstanceState' "RUNNING"

pattern InstanceState_TERMINATED :: InstanceState
pattern InstanceState_TERMINATED = InstanceState' "TERMINATED"

{-# COMPLETE
  InstanceState_AWAITING_FULFILLMENT,
  InstanceState_BOOTSTRAPPING,
  InstanceState_PROVISIONING,
  InstanceState_RUNNING,
  InstanceState_TERMINATED,
  InstanceState'
  #-}
