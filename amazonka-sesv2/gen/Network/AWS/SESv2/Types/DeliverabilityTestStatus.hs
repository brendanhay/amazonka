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
-- Module      : Network.AWS.SESv2.Types.DeliverabilityTestStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.DeliverabilityTestStatus
  ( DeliverabilityTestStatus
      ( ..,
        DeliverabilityTestStatus_COMPLETED,
        DeliverabilityTestStatus_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The status of a predictive inbox placement test. If the status is
-- @IN_PROGRESS@, then the predictive inbox placement test is currently
-- running. Predictive inbox placement tests are usually complete within 24
-- hours of creating the test. If the status is @COMPLETE@, then the test
-- is finished, and you can use the @GetDeliverabilityTestReport@ operation
-- to view the results of the test.
newtype DeliverabilityTestStatus = DeliverabilityTestStatus'
  { fromDeliverabilityTestStatus ::
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

pattern DeliverabilityTestStatus_COMPLETED :: DeliverabilityTestStatus
pattern DeliverabilityTestStatus_COMPLETED = DeliverabilityTestStatus' "COMPLETED"

pattern DeliverabilityTestStatus_IN_PROGRESS :: DeliverabilityTestStatus
pattern DeliverabilityTestStatus_IN_PROGRESS = DeliverabilityTestStatus' "IN_PROGRESS"

{-# COMPLETE
  DeliverabilityTestStatus_COMPLETED,
  DeliverabilityTestStatus_IN_PROGRESS,
  DeliverabilityTestStatus'
  #-}
