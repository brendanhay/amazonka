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
-- Module      : Amazonka.MediaConvert.Types.SimulateReservedQueue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.SimulateReservedQueue
  ( SimulateReservedQueue
      ( ..,
        SimulateReservedQueue_DISABLED,
        SimulateReservedQueue_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Enable this setting when you run a test job to estimate how many
-- reserved transcoding slots (RTS) you need. When this is enabled,
-- MediaConvert runs your job from an on-demand queue with similar
-- performance to what you will see with one RTS in a reserved queue. This
-- setting is disabled by default.
newtype SimulateReservedQueue = SimulateReservedQueue'
  { fromSimulateReservedQueue ::
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

pattern SimulateReservedQueue_DISABLED :: SimulateReservedQueue
pattern SimulateReservedQueue_DISABLED = SimulateReservedQueue' "DISABLED"

pattern SimulateReservedQueue_ENABLED :: SimulateReservedQueue
pattern SimulateReservedQueue_ENABLED = SimulateReservedQueue' "ENABLED"

{-# COMPLETE
  SimulateReservedQueue_DISABLED,
  SimulateReservedQueue_ENABLED,
  SimulateReservedQueue'
  #-}
