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
-- Module      : Amazonka.SageMaker.Types.ProductionVariantAcceleratorType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProductionVariantAcceleratorType
  ( ProductionVariantAcceleratorType
      ( ..,
        ProductionVariantAcceleratorType_Ml_eia1_large,
        ProductionVariantAcceleratorType_Ml_eia1_medium,
        ProductionVariantAcceleratorType_Ml_eia1_xlarge,
        ProductionVariantAcceleratorType_Ml_eia2_large,
        ProductionVariantAcceleratorType_Ml_eia2_medium,
        ProductionVariantAcceleratorType_Ml_eia2_xlarge
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProductionVariantAcceleratorType = ProductionVariantAcceleratorType'
  { fromProductionVariantAcceleratorType ::
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

pattern ProductionVariantAcceleratorType_Ml_eia1_large :: ProductionVariantAcceleratorType
pattern ProductionVariantAcceleratorType_Ml_eia1_large = ProductionVariantAcceleratorType' "ml.eia1.large"

pattern ProductionVariantAcceleratorType_Ml_eia1_medium :: ProductionVariantAcceleratorType
pattern ProductionVariantAcceleratorType_Ml_eia1_medium = ProductionVariantAcceleratorType' "ml.eia1.medium"

pattern ProductionVariantAcceleratorType_Ml_eia1_xlarge :: ProductionVariantAcceleratorType
pattern ProductionVariantAcceleratorType_Ml_eia1_xlarge = ProductionVariantAcceleratorType' "ml.eia1.xlarge"

pattern ProductionVariantAcceleratorType_Ml_eia2_large :: ProductionVariantAcceleratorType
pattern ProductionVariantAcceleratorType_Ml_eia2_large = ProductionVariantAcceleratorType' "ml.eia2.large"

pattern ProductionVariantAcceleratorType_Ml_eia2_medium :: ProductionVariantAcceleratorType
pattern ProductionVariantAcceleratorType_Ml_eia2_medium = ProductionVariantAcceleratorType' "ml.eia2.medium"

pattern ProductionVariantAcceleratorType_Ml_eia2_xlarge :: ProductionVariantAcceleratorType
pattern ProductionVariantAcceleratorType_Ml_eia2_xlarge = ProductionVariantAcceleratorType' "ml.eia2.xlarge"

{-# COMPLETE
  ProductionVariantAcceleratorType_Ml_eia1_large,
  ProductionVariantAcceleratorType_Ml_eia1_medium,
  ProductionVariantAcceleratorType_Ml_eia1_xlarge,
  ProductionVariantAcceleratorType_Ml_eia2_large,
  ProductionVariantAcceleratorType_Ml_eia2_medium,
  ProductionVariantAcceleratorType_Ml_eia2_xlarge,
  ProductionVariantAcceleratorType'
  #-}
