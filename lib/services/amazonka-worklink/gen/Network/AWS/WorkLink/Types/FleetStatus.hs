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
-- Module      : Network.AWS.WorkLink.Types.FleetStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkLink.Types.FleetStatus
  ( FleetStatus
      ( ..,
        FleetStatus_ACTIVE,
        FleetStatus_CREATING,
        FleetStatus_DELETED,
        FleetStatus_DELETING,
        FleetStatus_FAILED_TO_CREATE,
        FleetStatus_FAILED_TO_DELETE
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

pattern FleetStatus_ACTIVE :: FleetStatus
pattern FleetStatus_ACTIVE = FleetStatus' "ACTIVE"

pattern FleetStatus_CREATING :: FleetStatus
pattern FleetStatus_CREATING = FleetStatus' "CREATING"

pattern FleetStatus_DELETED :: FleetStatus
pattern FleetStatus_DELETED = FleetStatus' "DELETED"

pattern FleetStatus_DELETING :: FleetStatus
pattern FleetStatus_DELETING = FleetStatus' "DELETING"

pattern FleetStatus_FAILED_TO_CREATE :: FleetStatus
pattern FleetStatus_FAILED_TO_CREATE = FleetStatus' "FAILED_TO_CREATE"

pattern FleetStatus_FAILED_TO_DELETE :: FleetStatus
pattern FleetStatus_FAILED_TO_DELETE = FleetStatus' "FAILED_TO_DELETE"

{-# COMPLETE
  FleetStatus_ACTIVE,
  FleetStatus_CREATING,
  FleetStatus_DELETED,
  FleetStatus_DELETING,
  FleetStatus_FAILED_TO_CREATE,
  FleetStatus_FAILED_TO_DELETE,
  FleetStatus'
  #-}
