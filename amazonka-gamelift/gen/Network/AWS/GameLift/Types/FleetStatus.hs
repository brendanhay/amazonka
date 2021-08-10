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
-- Module      : Network.AWS.GameLift.Types.FleetStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.FleetStatus
  ( FleetStatus
      ( ..,
        FleetStatus_ACTIVATING,
        FleetStatus_ACTIVE,
        FleetStatus_BUILDING,
        FleetStatus_DELETING,
        FleetStatus_DOWNLOADING,
        FleetStatus_ERROR,
        FleetStatus_NEW,
        FleetStatus_TERMINATED,
        FleetStatus_VALIDATING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype FleetStatus = FleetStatus'
  { fromFleetStatus ::
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

pattern FleetStatus_ACTIVATING :: FleetStatus
pattern FleetStatus_ACTIVATING = FleetStatus' "ACTIVATING"

pattern FleetStatus_ACTIVE :: FleetStatus
pattern FleetStatus_ACTIVE = FleetStatus' "ACTIVE"

pattern FleetStatus_BUILDING :: FleetStatus
pattern FleetStatus_BUILDING = FleetStatus' "BUILDING"

pattern FleetStatus_DELETING :: FleetStatus
pattern FleetStatus_DELETING = FleetStatus' "DELETING"

pattern FleetStatus_DOWNLOADING :: FleetStatus
pattern FleetStatus_DOWNLOADING = FleetStatus' "DOWNLOADING"

pattern FleetStatus_ERROR :: FleetStatus
pattern FleetStatus_ERROR = FleetStatus' "ERROR"

pattern FleetStatus_NEW :: FleetStatus
pattern FleetStatus_NEW = FleetStatus' "NEW"

pattern FleetStatus_TERMINATED :: FleetStatus
pattern FleetStatus_TERMINATED = FleetStatus' "TERMINATED"

pattern FleetStatus_VALIDATING :: FleetStatus
pattern FleetStatus_VALIDATING = FleetStatus' "VALIDATING"

{-# COMPLETE
  FleetStatus_ACTIVATING,
  FleetStatus_ACTIVE,
  FleetStatus_BUILDING,
  FleetStatus_DELETING,
  FleetStatus_DOWNLOADING,
  FleetStatus_ERROR,
  FleetStatus_NEW,
  FleetStatus_TERMINATED,
  FleetStatus_VALIDATING,
  FleetStatus'
  #-}
