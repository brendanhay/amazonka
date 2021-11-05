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
-- Module      : Amazonka.StepFunctions.Types.StateMachineStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.StateMachineStatus
  ( StateMachineStatus
      ( ..,
        StateMachineStatus_ACTIVE,
        StateMachineStatus_DELETING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype StateMachineStatus = StateMachineStatus'
  { fromStateMachineStatus ::
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

pattern StateMachineStatus_ACTIVE :: StateMachineStatus
pattern StateMachineStatus_ACTIVE = StateMachineStatus' "ACTIVE"

pattern StateMachineStatus_DELETING :: StateMachineStatus
pattern StateMachineStatus_DELETING = StateMachineStatus' "DELETING"

{-# COMPLETE
  StateMachineStatus_ACTIVE,
  StateMachineStatus_DELETING,
  StateMachineStatus'
  #-}
