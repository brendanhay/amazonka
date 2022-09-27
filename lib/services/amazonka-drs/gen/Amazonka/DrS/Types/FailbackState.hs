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
-- Module      : Amazonka.DrS.Types.FailbackState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.FailbackState
  ( FailbackState
      ( ..,
        FailbackState_FAILBACK_COMPLETED,
        FailbackState_FAILBACK_ERROR,
        FailbackState_FAILBACK_IN_PROGRESS,
        FailbackState_FAILBACK_NOT_STARTED,
        FailbackState_FAILBACK_READY_FOR_LAUNCH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype FailbackState = FailbackState'
  { fromFailbackState ::
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

pattern FailbackState_FAILBACK_COMPLETED :: FailbackState
pattern FailbackState_FAILBACK_COMPLETED = FailbackState' "FAILBACK_COMPLETED"

pattern FailbackState_FAILBACK_ERROR :: FailbackState
pattern FailbackState_FAILBACK_ERROR = FailbackState' "FAILBACK_ERROR"

pattern FailbackState_FAILBACK_IN_PROGRESS :: FailbackState
pattern FailbackState_FAILBACK_IN_PROGRESS = FailbackState' "FAILBACK_IN_PROGRESS"

pattern FailbackState_FAILBACK_NOT_STARTED :: FailbackState
pattern FailbackState_FAILBACK_NOT_STARTED = FailbackState' "FAILBACK_NOT_STARTED"

pattern FailbackState_FAILBACK_READY_FOR_LAUNCH :: FailbackState
pattern FailbackState_FAILBACK_READY_FOR_LAUNCH = FailbackState' "FAILBACK_READY_FOR_LAUNCH"

{-# COMPLETE
  FailbackState_FAILBACK_COMPLETED,
  FailbackState_FAILBACK_ERROR,
  FailbackState_FAILBACK_IN_PROGRESS,
  FailbackState_FAILBACK_NOT_STARTED,
  FailbackState_FAILBACK_READY_FOR_LAUNCH,
  FailbackState'
  #-}
