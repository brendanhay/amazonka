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
-- Module      : Amazonka.MGN.Types.LifeCycleState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.LifeCycleState
  ( LifeCycleState
      ( ..,
        LifeCycleState_CUTOVER,
        LifeCycleState_CUTTING_OVER,
        LifeCycleState_DISCONNECTED,
        LifeCycleState_DISCOVERED,
        LifeCycleState_NOT_READY,
        LifeCycleState_READY_FOR_CUTOVER,
        LifeCycleState_READY_FOR_TEST,
        LifeCycleState_STOPPED,
        LifeCycleState_TESTING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LifeCycleState = LifeCycleState'
  { fromLifeCycleState ::
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

pattern LifeCycleState_CUTOVER :: LifeCycleState
pattern LifeCycleState_CUTOVER = LifeCycleState' "CUTOVER"

pattern LifeCycleState_CUTTING_OVER :: LifeCycleState
pattern LifeCycleState_CUTTING_OVER = LifeCycleState' "CUTTING_OVER"

pattern LifeCycleState_DISCONNECTED :: LifeCycleState
pattern LifeCycleState_DISCONNECTED = LifeCycleState' "DISCONNECTED"

pattern LifeCycleState_DISCOVERED :: LifeCycleState
pattern LifeCycleState_DISCOVERED = LifeCycleState' "DISCOVERED"

pattern LifeCycleState_NOT_READY :: LifeCycleState
pattern LifeCycleState_NOT_READY = LifeCycleState' "NOT_READY"

pattern LifeCycleState_READY_FOR_CUTOVER :: LifeCycleState
pattern LifeCycleState_READY_FOR_CUTOVER = LifeCycleState' "READY_FOR_CUTOVER"

pattern LifeCycleState_READY_FOR_TEST :: LifeCycleState
pattern LifeCycleState_READY_FOR_TEST = LifeCycleState' "READY_FOR_TEST"

pattern LifeCycleState_STOPPED :: LifeCycleState
pattern LifeCycleState_STOPPED = LifeCycleState' "STOPPED"

pattern LifeCycleState_TESTING :: LifeCycleState
pattern LifeCycleState_TESTING = LifeCycleState' "TESTING"

{-# COMPLETE
  LifeCycleState_CUTOVER,
  LifeCycleState_CUTTING_OVER,
  LifeCycleState_DISCONNECTED,
  LifeCycleState_DISCOVERED,
  LifeCycleState_NOT_READY,
  LifeCycleState_READY_FOR_CUTOVER,
  LifeCycleState_READY_FOR_TEST,
  LifeCycleState_STOPPED,
  LifeCycleState_TESTING,
  LifeCycleState'
  #-}
