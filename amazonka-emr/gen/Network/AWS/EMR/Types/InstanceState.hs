{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype InstanceState = InstanceState'
  { fromInstanceState ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
