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
-- Module      : Network.AWS.EMR.Types.InstanceState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceState
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

import qualified Network.AWS.Core as Core

newtype InstanceState = InstanceState'
  { fromInstanceState ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
