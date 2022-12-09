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
-- Module      : Amazonka.SimSpaceWeaver.Types.ClockTargetStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SimSpaceWeaver.Types.ClockTargetStatus
  ( ClockTargetStatus
      ( ..,
        ClockTargetStatus_STARTED,
        ClockTargetStatus_STOPPED,
        ClockTargetStatus_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ClockTargetStatus = ClockTargetStatus'
  { fromClockTargetStatus ::
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

pattern ClockTargetStatus_STARTED :: ClockTargetStatus
pattern ClockTargetStatus_STARTED = ClockTargetStatus' "STARTED"

pattern ClockTargetStatus_STOPPED :: ClockTargetStatus
pattern ClockTargetStatus_STOPPED = ClockTargetStatus' "STOPPED"

pattern ClockTargetStatus_UNKNOWN :: ClockTargetStatus
pattern ClockTargetStatus_UNKNOWN = ClockTargetStatus' "UNKNOWN"

{-# COMPLETE
  ClockTargetStatus_STARTED,
  ClockTargetStatus_STOPPED,
  ClockTargetStatus_UNKNOWN,
  ClockTargetStatus'
  #-}
