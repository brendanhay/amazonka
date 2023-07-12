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
-- Module      : Amazonka.SageMaker.Types.VariantPropertyType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.VariantPropertyType
  ( VariantPropertyType
      ( ..,
        VariantPropertyType_DataCaptureConfig,
        VariantPropertyType_DesiredInstanceCount,
        VariantPropertyType_DesiredWeight
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VariantPropertyType = VariantPropertyType'
  { fromVariantPropertyType ::
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

pattern VariantPropertyType_DataCaptureConfig :: VariantPropertyType
pattern VariantPropertyType_DataCaptureConfig = VariantPropertyType' "DataCaptureConfig"

pattern VariantPropertyType_DesiredInstanceCount :: VariantPropertyType
pattern VariantPropertyType_DesiredInstanceCount = VariantPropertyType' "DesiredInstanceCount"

pattern VariantPropertyType_DesiredWeight :: VariantPropertyType
pattern VariantPropertyType_DesiredWeight = VariantPropertyType' "DesiredWeight"

{-# COMPLETE
  VariantPropertyType_DataCaptureConfig,
  VariantPropertyType_DesiredInstanceCount,
  VariantPropertyType_DesiredWeight,
  VariantPropertyType'
  #-}
