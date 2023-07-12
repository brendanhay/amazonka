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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.FailbackState
  ( FailbackState
      ( ..,
        FailbackState_FAILBACK_COMPLETED,
        FailbackState_FAILBACK_ERROR,
        FailbackState_FAILBACK_IN_PROGRESS,
        FailbackState_FAILBACK_LAUNCH_STATE_NOT_AVAILABLE,
        FailbackState_FAILBACK_NOT_READY_FOR_LAUNCH,
        FailbackState_FAILBACK_NOT_STARTED,
        FailbackState_FAILBACK_READY_FOR_LAUNCH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FailbackState = FailbackState'
  { fromFailbackState ::
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

pattern FailbackState_FAILBACK_COMPLETED :: FailbackState
pattern FailbackState_FAILBACK_COMPLETED = FailbackState' "FAILBACK_COMPLETED"

pattern FailbackState_FAILBACK_ERROR :: FailbackState
pattern FailbackState_FAILBACK_ERROR = FailbackState' "FAILBACK_ERROR"

pattern FailbackState_FAILBACK_IN_PROGRESS :: FailbackState
pattern FailbackState_FAILBACK_IN_PROGRESS = FailbackState' "FAILBACK_IN_PROGRESS"

pattern FailbackState_FAILBACK_LAUNCH_STATE_NOT_AVAILABLE :: FailbackState
pattern FailbackState_FAILBACK_LAUNCH_STATE_NOT_AVAILABLE = FailbackState' "FAILBACK_LAUNCH_STATE_NOT_AVAILABLE"

pattern FailbackState_FAILBACK_NOT_READY_FOR_LAUNCH :: FailbackState
pattern FailbackState_FAILBACK_NOT_READY_FOR_LAUNCH = FailbackState' "FAILBACK_NOT_READY_FOR_LAUNCH"

pattern FailbackState_FAILBACK_NOT_STARTED :: FailbackState
pattern FailbackState_FAILBACK_NOT_STARTED = FailbackState' "FAILBACK_NOT_STARTED"

pattern FailbackState_FAILBACK_READY_FOR_LAUNCH :: FailbackState
pattern FailbackState_FAILBACK_READY_FOR_LAUNCH = FailbackState' "FAILBACK_READY_FOR_LAUNCH"

{-# COMPLETE
  FailbackState_FAILBACK_COMPLETED,
  FailbackState_FAILBACK_ERROR,
  FailbackState_FAILBACK_IN_PROGRESS,
  FailbackState_FAILBACK_LAUNCH_STATE_NOT_AVAILABLE,
  FailbackState_FAILBACK_NOT_READY_FOR_LAUNCH,
  FailbackState_FAILBACK_NOT_STARTED,
  FailbackState_FAILBACK_READY_FOR_LAUNCH,
  FailbackState'
  #-}
