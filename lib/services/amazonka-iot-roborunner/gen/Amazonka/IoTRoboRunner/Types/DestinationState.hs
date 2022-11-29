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
-- Module      : Amazonka.IoTRoboRunner.Types.DestinationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTRoboRunner.Types.DestinationState
  ( DestinationState
      ( ..,
        DestinationState_DECOMMISSIONED,
        DestinationState_DISABLED,
        DestinationState_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | State of the destination.
newtype DestinationState = DestinationState'
  { fromDestinationState ::
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

pattern DestinationState_DECOMMISSIONED :: DestinationState
pattern DestinationState_DECOMMISSIONED = DestinationState' "DECOMMISSIONED"

pattern DestinationState_DISABLED :: DestinationState
pattern DestinationState_DISABLED = DestinationState' "DISABLED"

pattern DestinationState_ENABLED :: DestinationState
pattern DestinationState_ENABLED = DestinationState' "ENABLED"

{-# COMPLETE
  DestinationState_DECOMMISSIONED,
  DestinationState_DISABLED,
  DestinationState_ENABLED,
  DestinationState'
  #-}
