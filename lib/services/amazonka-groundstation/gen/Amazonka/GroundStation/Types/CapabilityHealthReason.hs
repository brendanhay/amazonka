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
-- Module      : Amazonka.GroundStation.Types.CapabilityHealthReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.CapabilityHealthReason
  ( CapabilityHealthReason
      ( ..,
        CapabilityHealthReason_DATAPLANE_FAILURE,
        CapabilityHealthReason_HEALTHY,
        CapabilityHealthReason_INITIALIZING_DATAPLANE,
        CapabilityHealthReason_INVALID_IP_OWNERSHIP,
        CapabilityHealthReason_NOT_AUTHORIZED_TO_CREATE_SLR,
        CapabilityHealthReason_NO_REGISTERED_AGENT,
        CapabilityHealthReason_UNVERIFIED_IP_OWNERSHIP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CapabilityHealthReason = CapabilityHealthReason'
  { fromCapabilityHealthReason ::
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

pattern CapabilityHealthReason_DATAPLANE_FAILURE :: CapabilityHealthReason
pattern CapabilityHealthReason_DATAPLANE_FAILURE = CapabilityHealthReason' "DATAPLANE_FAILURE"

pattern CapabilityHealthReason_HEALTHY :: CapabilityHealthReason
pattern CapabilityHealthReason_HEALTHY = CapabilityHealthReason' "HEALTHY"

pattern CapabilityHealthReason_INITIALIZING_DATAPLANE :: CapabilityHealthReason
pattern CapabilityHealthReason_INITIALIZING_DATAPLANE = CapabilityHealthReason' "INITIALIZING_DATAPLANE"

pattern CapabilityHealthReason_INVALID_IP_OWNERSHIP :: CapabilityHealthReason
pattern CapabilityHealthReason_INVALID_IP_OWNERSHIP = CapabilityHealthReason' "INVALID_IP_OWNERSHIP"

pattern CapabilityHealthReason_NOT_AUTHORIZED_TO_CREATE_SLR :: CapabilityHealthReason
pattern CapabilityHealthReason_NOT_AUTHORIZED_TO_CREATE_SLR = CapabilityHealthReason' "NOT_AUTHORIZED_TO_CREATE_SLR"

pattern CapabilityHealthReason_NO_REGISTERED_AGENT :: CapabilityHealthReason
pattern CapabilityHealthReason_NO_REGISTERED_AGENT = CapabilityHealthReason' "NO_REGISTERED_AGENT"

pattern CapabilityHealthReason_UNVERIFIED_IP_OWNERSHIP :: CapabilityHealthReason
pattern CapabilityHealthReason_UNVERIFIED_IP_OWNERSHIP = CapabilityHealthReason' "UNVERIFIED_IP_OWNERSHIP"

{-# COMPLETE
  CapabilityHealthReason_DATAPLANE_FAILURE,
  CapabilityHealthReason_HEALTHY,
  CapabilityHealthReason_INITIALIZING_DATAPLANE,
  CapabilityHealthReason_INVALID_IP_OWNERSHIP,
  CapabilityHealthReason_NOT_AUTHORIZED_TO_CREATE_SLR,
  CapabilityHealthReason_NO_REGISTERED_AGENT,
  CapabilityHealthReason_UNVERIFIED_IP_OWNERSHIP,
  CapabilityHealthReason'
  #-}
