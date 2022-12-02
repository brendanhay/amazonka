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
-- Module      : Amazonka.CloudWatchEvents.Types.ReplayState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.ReplayState
  ( ReplayState
      ( ..,
        ReplayState_CANCELLED,
        ReplayState_CANCELLING,
        ReplayState_COMPLETED,
        ReplayState_FAILED,
        ReplayState_RUNNING,
        ReplayState_STARTING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReplayState = ReplayState'
  { fromReplayState ::
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

pattern ReplayState_CANCELLED :: ReplayState
pattern ReplayState_CANCELLED = ReplayState' "CANCELLED"

pattern ReplayState_CANCELLING :: ReplayState
pattern ReplayState_CANCELLING = ReplayState' "CANCELLING"

pattern ReplayState_COMPLETED :: ReplayState
pattern ReplayState_COMPLETED = ReplayState' "COMPLETED"

pattern ReplayState_FAILED :: ReplayState
pattern ReplayState_FAILED = ReplayState' "FAILED"

pattern ReplayState_RUNNING :: ReplayState
pattern ReplayState_RUNNING = ReplayState' "RUNNING"

pattern ReplayState_STARTING :: ReplayState
pattern ReplayState_STARTING = ReplayState' "STARTING"

{-# COMPLETE
  ReplayState_CANCELLED,
  ReplayState_CANCELLING,
  ReplayState_COMPLETED,
  ReplayState_FAILED,
  ReplayState_RUNNING,
  ReplayState_STARTING,
  ReplayState'
  #-}
