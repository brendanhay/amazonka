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
-- Module      : Amazonka.PinpointEmail.Types.DeliverabilityDashboardAccountStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.DeliverabilityDashboardAccountStatus
  ( DeliverabilityDashboardAccountStatus
      ( ..,
        DeliverabilityDashboardAccountStatus_ACTIVE,
        DeliverabilityDashboardAccountStatus_DISABLED,
        DeliverabilityDashboardAccountStatus_PENDING_EXPIRATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The current status of your Deliverability dashboard subscription. If
-- this value is @PENDING_EXPIRATION@, your subscription is scheduled to
-- expire at the end of the current calendar month.
newtype DeliverabilityDashboardAccountStatus = DeliverabilityDashboardAccountStatus'
  { fromDeliverabilityDashboardAccountStatus ::
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

pattern DeliverabilityDashboardAccountStatus_ACTIVE :: DeliverabilityDashboardAccountStatus
pattern DeliverabilityDashboardAccountStatus_ACTIVE = DeliverabilityDashboardAccountStatus' "ACTIVE"

pattern DeliverabilityDashboardAccountStatus_DISABLED :: DeliverabilityDashboardAccountStatus
pattern DeliverabilityDashboardAccountStatus_DISABLED = DeliverabilityDashboardAccountStatus' "DISABLED"

pattern DeliverabilityDashboardAccountStatus_PENDING_EXPIRATION :: DeliverabilityDashboardAccountStatus
pattern DeliverabilityDashboardAccountStatus_PENDING_EXPIRATION = DeliverabilityDashboardAccountStatus' "PENDING_EXPIRATION"

{-# COMPLETE
  DeliverabilityDashboardAccountStatus_ACTIVE,
  DeliverabilityDashboardAccountStatus_DISABLED,
  DeliverabilityDashboardAccountStatus_PENDING_EXPIRATION,
  DeliverabilityDashboardAccountStatus'
  #-}
