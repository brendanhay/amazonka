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
-- Module      : Amazonka.SavingsPlans.Types.SavingsPlanRateFilterAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlanRateFilterAttribute
  ( SavingsPlanRateFilterAttribute
      ( ..,
        SavingsPlanRateFilterAttribute_InstanceFamily,
        SavingsPlanRateFilterAttribute_InstanceType,
        SavingsPlanRateFilterAttribute_ProductDescription,
        SavingsPlanRateFilterAttribute_ProductId,
        SavingsPlanRateFilterAttribute_Region,
        SavingsPlanRateFilterAttribute_Tenancy
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SavingsPlanRateFilterAttribute = SavingsPlanRateFilterAttribute'
  { fromSavingsPlanRateFilterAttribute ::
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

pattern SavingsPlanRateFilterAttribute_InstanceFamily :: SavingsPlanRateFilterAttribute
pattern SavingsPlanRateFilterAttribute_InstanceFamily = SavingsPlanRateFilterAttribute' "instanceFamily"

pattern SavingsPlanRateFilterAttribute_InstanceType :: SavingsPlanRateFilterAttribute
pattern SavingsPlanRateFilterAttribute_InstanceType = SavingsPlanRateFilterAttribute' "instanceType"

pattern SavingsPlanRateFilterAttribute_ProductDescription :: SavingsPlanRateFilterAttribute
pattern SavingsPlanRateFilterAttribute_ProductDescription = SavingsPlanRateFilterAttribute' "productDescription"

pattern SavingsPlanRateFilterAttribute_ProductId :: SavingsPlanRateFilterAttribute
pattern SavingsPlanRateFilterAttribute_ProductId = SavingsPlanRateFilterAttribute' "productId"

pattern SavingsPlanRateFilterAttribute_Region :: SavingsPlanRateFilterAttribute
pattern SavingsPlanRateFilterAttribute_Region = SavingsPlanRateFilterAttribute' "region"

pattern SavingsPlanRateFilterAttribute_Tenancy :: SavingsPlanRateFilterAttribute
pattern SavingsPlanRateFilterAttribute_Tenancy = SavingsPlanRateFilterAttribute' "tenancy"

{-# COMPLETE
  SavingsPlanRateFilterAttribute_InstanceFamily,
  SavingsPlanRateFilterAttribute_InstanceType,
  SavingsPlanRateFilterAttribute_ProductDescription,
  SavingsPlanRateFilterAttribute_ProductId,
  SavingsPlanRateFilterAttribute_Region,
  SavingsPlanRateFilterAttribute_Tenancy,
  SavingsPlanRateFilterAttribute'
  #-}
