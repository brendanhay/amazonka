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
-- Module      : Network.AWS.SageMaker.Types.VariantPropertyType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.VariantPropertyType
  ( VariantPropertyType
      ( ..,
        VariantPropertyType_DataCaptureConfig,
        VariantPropertyType_DesiredInstanceCount,
        VariantPropertyType_DesiredWeight
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype VariantPropertyType = VariantPropertyType'
  { fromVariantPropertyType ::
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
