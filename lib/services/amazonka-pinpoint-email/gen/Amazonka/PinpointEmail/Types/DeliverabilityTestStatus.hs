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
-- Module      : Amazonka.PinpointEmail.Types.DeliverabilityTestStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.DeliverabilityTestStatus
  ( DeliverabilityTestStatus
      ( ..,
        DeliverabilityTestStatus_COMPLETED,
        DeliverabilityTestStatus_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The status of a predictive inbox placement test. If the status is
-- @IN_PROGRESS@, then the predictive inbox placement test is currently
-- running. Predictive inbox placement tests are usually complete within 24
-- hours of creating the test. If the status is @COMPLETE@, then the test
-- is finished, and you can use the @GetDeliverabilityTestReport@ operation
-- to view the results of the test.
newtype DeliverabilityTestStatus = DeliverabilityTestStatus'
  { fromDeliverabilityTestStatus ::
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

pattern DeliverabilityTestStatus_COMPLETED :: DeliverabilityTestStatus
pattern DeliverabilityTestStatus_COMPLETED = DeliverabilityTestStatus' "COMPLETED"

pattern DeliverabilityTestStatus_IN_PROGRESS :: DeliverabilityTestStatus
pattern DeliverabilityTestStatus_IN_PROGRESS = DeliverabilityTestStatus' "IN_PROGRESS"

{-# COMPLETE
  DeliverabilityTestStatus_COMPLETED,
  DeliverabilityTestStatus_IN_PROGRESS,
  DeliverabilityTestStatus'
  #-}
