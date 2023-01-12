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
-- Module      : Amazonka.EC2.Types.FleetActivityStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FleetActivityStatus
  ( FleetActivityStatus
      ( ..,
        FleetActivityStatus_Error,
        FleetActivityStatus_Fulfilled,
        FleetActivityStatus_Pending_fulfillment,
        FleetActivityStatus_Pending_termination
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype FleetActivityStatus = FleetActivityStatus'
  { fromFleetActivityStatus ::
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

pattern FleetActivityStatus_Error :: FleetActivityStatus
pattern FleetActivityStatus_Error = FleetActivityStatus' "error"

pattern FleetActivityStatus_Fulfilled :: FleetActivityStatus
pattern FleetActivityStatus_Fulfilled = FleetActivityStatus' "fulfilled"

pattern FleetActivityStatus_Pending_fulfillment :: FleetActivityStatus
pattern FleetActivityStatus_Pending_fulfillment = FleetActivityStatus' "pending_fulfillment"

pattern FleetActivityStatus_Pending_termination :: FleetActivityStatus
pattern FleetActivityStatus_Pending_termination = FleetActivityStatus' "pending_termination"

{-# COMPLETE
  FleetActivityStatus_Error,
  FleetActivityStatus_Fulfilled,
  FleetActivityStatus_Pending_fulfillment,
  FleetActivityStatus_Pending_termination,
  FleetActivityStatus'
  #-}
