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
-- Module      : Amazonka.SavingsPlans.Types.SavingsPlanState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlanState
  ( SavingsPlanState
      ( ..,
        SavingsPlanState_Active,
        SavingsPlanState_Payment_failed,
        SavingsPlanState_Payment_pending,
        SavingsPlanState_Queued,
        SavingsPlanState_Queued_deleted,
        SavingsPlanState_Retired
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SavingsPlanState = SavingsPlanState'
  { fromSavingsPlanState ::
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

pattern SavingsPlanState_Active :: SavingsPlanState
pattern SavingsPlanState_Active = SavingsPlanState' "active"

pattern SavingsPlanState_Payment_failed :: SavingsPlanState
pattern SavingsPlanState_Payment_failed = SavingsPlanState' "payment-failed"

pattern SavingsPlanState_Payment_pending :: SavingsPlanState
pattern SavingsPlanState_Payment_pending = SavingsPlanState' "payment-pending"

pattern SavingsPlanState_Queued :: SavingsPlanState
pattern SavingsPlanState_Queued = SavingsPlanState' "queued"

pattern SavingsPlanState_Queued_deleted :: SavingsPlanState
pattern SavingsPlanState_Queued_deleted = SavingsPlanState' "queued-deleted"

pattern SavingsPlanState_Retired :: SavingsPlanState
pattern SavingsPlanState_Retired = SavingsPlanState' "retired"

{-# COMPLETE
  SavingsPlanState_Active,
  SavingsPlanState_Payment_failed,
  SavingsPlanState_Payment_pending,
  SavingsPlanState_Queued,
  SavingsPlanState_Queued_deleted,
  SavingsPlanState_Retired,
  SavingsPlanState'
  #-}
