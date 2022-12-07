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
-- Module      : Amazonka.EMR.Types.StepState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.StepState
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StepState = StepState'
  { fromStepState ::
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
