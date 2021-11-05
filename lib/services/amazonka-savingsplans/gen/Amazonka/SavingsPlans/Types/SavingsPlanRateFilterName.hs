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
-- Module      : Amazonka.SavingsPlans.Types.SavingsPlanRateFilterName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlanRateFilterName
  ( SavingsPlanRateFilterName
      ( ..,
        SavingsPlanRateFilterName_InstanceType,
        SavingsPlanRateFilterName_Operation,
        SavingsPlanRateFilterName_ProductDescription,
        SavingsPlanRateFilterName_ProductType,
        SavingsPlanRateFilterName_Region,
        SavingsPlanRateFilterName_ServiceCode,
        SavingsPlanRateFilterName_Tenancy,
        SavingsPlanRateFilterName_UsageType
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SavingsPlanRateFilterName = SavingsPlanRateFilterName'
  { fromSavingsPlanRateFilterName ::
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

pattern SavingsPlanRateFilterName_InstanceType :: SavingsPlanRateFilterName
pattern SavingsPlanRateFilterName_InstanceType = SavingsPlanRateFilterName' "instanceType"

pattern SavingsPlanRateFilterName_Operation :: SavingsPlanRateFilterName
pattern SavingsPlanRateFilterName_Operation = SavingsPlanRateFilterName' "operation"

pattern SavingsPlanRateFilterName_ProductDescription :: SavingsPlanRateFilterName
pattern SavingsPlanRateFilterName_ProductDescription = SavingsPlanRateFilterName' "productDescription"

pattern SavingsPlanRateFilterName_ProductType :: SavingsPlanRateFilterName
pattern SavingsPlanRateFilterName_ProductType = SavingsPlanRateFilterName' "productType"

pattern SavingsPlanRateFilterName_Region :: SavingsPlanRateFilterName
pattern SavingsPlanRateFilterName_Region = SavingsPlanRateFilterName' "region"

pattern SavingsPlanRateFilterName_ServiceCode :: SavingsPlanRateFilterName
pattern SavingsPlanRateFilterName_ServiceCode = SavingsPlanRateFilterName' "serviceCode"

pattern SavingsPlanRateFilterName_Tenancy :: SavingsPlanRateFilterName
pattern SavingsPlanRateFilterName_Tenancy = SavingsPlanRateFilterName' "tenancy"

pattern SavingsPlanRateFilterName_UsageType :: SavingsPlanRateFilterName
pattern SavingsPlanRateFilterName_UsageType = SavingsPlanRateFilterName' "usageType"

{-# COMPLETE
  SavingsPlanRateFilterName_InstanceType,
  SavingsPlanRateFilterName_Operation,
  SavingsPlanRateFilterName_ProductDescription,
  SavingsPlanRateFilterName_ProductType,
  SavingsPlanRateFilterName_Region,
  SavingsPlanRateFilterName_ServiceCode,
  SavingsPlanRateFilterName_Tenancy,
  SavingsPlanRateFilterName_UsageType,
  SavingsPlanRateFilterName'
  #-}
