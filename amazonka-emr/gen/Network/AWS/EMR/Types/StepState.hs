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
-- Module      : Network.AWS.EMR.Types.StepState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepState
  ( StepState
      ( ..,
        StepState_CANCELLED,
        StepState_CANCEL_PENDING,
        StepState_COMPLETED,
        StepState_FAILED,
        StepState_INTERRUPTED,
        StepState_PENDING,
        StepState_RUNNING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype StepState = StepState'
  { fromStepState ::
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

pattern StepState_CANCELLED :: StepState
pattern StepState_CANCELLED = StepState' "CANCELLED"

pattern StepState_CANCEL_PENDING :: StepState
pattern StepState_CANCEL_PENDING = StepState' "CANCEL_PENDING"

pattern StepState_COMPLETED :: StepState
pattern StepState_COMPLETED = StepState' "COMPLETED"

pattern StepState_FAILED :: StepState
pattern StepState_FAILED = StepState' "FAILED"

pattern StepState_INTERRUPTED :: StepState
pattern StepState_INTERRUPTED = StepState' "INTERRUPTED"

pattern StepState_PENDING :: StepState
pattern StepState_PENDING = StepState' "PENDING"

pattern StepState_RUNNING :: StepState
pattern StepState_RUNNING = StepState' "RUNNING"

{-# COMPLETE
  StepState_CANCELLED,
  StepState_CANCEL_PENDING,
  StepState_COMPLETED,
  StepState_FAILED,
  StepState_INTERRUPTED,
  StepState_PENDING,
  StepState_RUNNING,
  StepState'
  #-}
