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
-- Module      : Amazonka.SimSpaceWeaver.Types.ClockStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.ClockStatus
  ( ClockStatus
      ( ..,
        ClockStatus_STARTED,
        ClockStatus_STARTING,
        ClockStatus_STOPPED,
        ClockStatus_STOPPING,
        ClockStatus_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ClockStatus = ClockStatus'
  { fromClockStatus ::
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

pattern ClockStatus_STARTED :: ClockStatus
pattern ClockStatus_STARTED = ClockStatus' "STARTED"

pattern ClockStatus_STARTING :: ClockStatus
pattern ClockStatus_STARTING = ClockStatus' "STARTING"

pattern ClockStatus_STOPPED :: ClockStatus
pattern ClockStatus_STOPPED = ClockStatus' "STOPPED"

pattern ClockStatus_STOPPING :: ClockStatus
pattern ClockStatus_STOPPING = ClockStatus' "STOPPING"

pattern ClockStatus_UNKNOWN :: ClockStatus
pattern ClockStatus_UNKNOWN = ClockStatus' "UNKNOWN"

{-# COMPLETE
  ClockStatus_STARTED,
  ClockStatus_STARTING,
  ClockStatus_STOPPED,
  ClockStatus_STOPPING,
  ClockStatus_UNKNOWN,
  ClockStatus'
  #-}
