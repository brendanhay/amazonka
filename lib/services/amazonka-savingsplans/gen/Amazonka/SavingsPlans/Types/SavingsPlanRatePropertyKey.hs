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
-- Module      : Amazonka.SavingsPlans.Types.SavingsPlanRatePropertyKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlanRatePropertyKey
  ( SavingsPlanRatePropertyKey
      ( ..,
        SavingsPlanRatePropertyKey_InstanceFamily,
        SavingsPlanRatePropertyKey_InstanceType,
        SavingsPlanRatePropertyKey_ProductDescription,
        SavingsPlanRatePropertyKey_Region,
        SavingsPlanRatePropertyKey_Tenancy
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SavingsPlanRatePropertyKey = SavingsPlanRatePropertyKey'
  { fromSavingsPlanRatePropertyKey ::
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

pattern SavingsPlanRatePropertyKey_InstanceFamily :: SavingsPlanRatePropertyKey
pattern SavingsPlanRatePropertyKey_InstanceFamily = SavingsPlanRatePropertyKey' "instanceFamily"

pattern SavingsPlanRatePropertyKey_InstanceType :: SavingsPlanRatePropertyKey
pattern SavingsPlanRatePropertyKey_InstanceType = SavingsPlanRatePropertyKey' "instanceType"

pattern SavingsPlanRatePropertyKey_ProductDescription :: SavingsPlanRatePropertyKey
pattern SavingsPlanRatePropertyKey_ProductDescription = SavingsPlanRatePropertyKey' "productDescription"

pattern SavingsPlanRatePropertyKey_Region :: SavingsPlanRatePropertyKey
pattern SavingsPlanRatePropertyKey_Region = SavingsPlanRatePropertyKey' "region"

pattern SavingsPlanRatePropertyKey_Tenancy :: SavingsPlanRatePropertyKey
pattern SavingsPlanRatePropertyKey_Tenancy = SavingsPlanRatePropertyKey' "tenancy"

{-# COMPLETE
  SavingsPlanRatePropertyKey_InstanceFamily,
  SavingsPlanRatePropertyKey_InstanceType,
  SavingsPlanRatePropertyKey_ProductDescription,
  SavingsPlanRatePropertyKey_Region,
  SavingsPlanRatePropertyKey_Tenancy,
  SavingsPlanRatePropertyKey'
  #-}
