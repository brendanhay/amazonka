{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProductionVariantAcceleratorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProductionVariantAcceleratorType
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

import qualified Network.AWS.Prelude as Prelude

newtype ProductionVariantAcceleratorType = ProductionVariantAcceleratorType'
  { fromProductionVariantAcceleratorType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
