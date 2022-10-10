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
-- Module      : Amazonka.SavingsPlans.Types.SavingsPlanRateUnit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlanRateUnit
  ( SavingsPlanRateUnit
      ( ..,
        SavingsPlanRateUnit_Hrs,
        SavingsPlanRateUnit_Lambda_GB_Second,
        SavingsPlanRateUnit_Request
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SavingsPlanRateUnit = SavingsPlanRateUnit'
  { fromSavingsPlanRateUnit ::
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

pattern SavingsPlanRateUnit_Hrs :: SavingsPlanRateUnit
pattern SavingsPlanRateUnit_Hrs = SavingsPlanRateUnit' "Hrs"

pattern SavingsPlanRateUnit_Lambda_GB_Second :: SavingsPlanRateUnit
pattern SavingsPlanRateUnit_Lambda_GB_Second = SavingsPlanRateUnit' "Lambda-GB-Second"

pattern SavingsPlanRateUnit_Request :: SavingsPlanRateUnit
pattern SavingsPlanRateUnit_Request = SavingsPlanRateUnit' "Request"

{-# COMPLETE
  SavingsPlanRateUnit_Hrs,
  SavingsPlanRateUnit_Lambda_GB_Second,
  SavingsPlanRateUnit_Request,
  SavingsPlanRateUnit'
  #-}
