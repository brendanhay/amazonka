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
-- Module      : Amazonka.StorageGateway.Types.AvailabilityMonitorTestStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.AvailabilityMonitorTestStatus
  ( AvailabilityMonitorTestStatus
      ( ..,
        AvailabilityMonitorTestStatus_COMPLETE,
        AvailabilityMonitorTestStatus_FAILED,
        AvailabilityMonitorTestStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AvailabilityMonitorTestStatus = AvailabilityMonitorTestStatus'
  { fromAvailabilityMonitorTestStatus ::
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

pattern AvailabilityMonitorTestStatus_COMPLETE :: AvailabilityMonitorTestStatus
pattern AvailabilityMonitorTestStatus_COMPLETE = AvailabilityMonitorTestStatus' "COMPLETE"

pattern AvailabilityMonitorTestStatus_FAILED :: AvailabilityMonitorTestStatus
pattern AvailabilityMonitorTestStatus_FAILED = AvailabilityMonitorTestStatus' "FAILED"

pattern AvailabilityMonitorTestStatus_PENDING :: AvailabilityMonitorTestStatus
pattern AvailabilityMonitorTestStatus_PENDING = AvailabilityMonitorTestStatus' "PENDING"

{-# COMPLETE
  AvailabilityMonitorTestStatus_COMPLETE,
  AvailabilityMonitorTestStatus_FAILED,
  AvailabilityMonitorTestStatus_PENDING,
  AvailabilityMonitorTestStatus'
  #-}
